{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)

import qualified Data.ByteString.Char8 as C8
import Data.Char
import Data.Word
import Data.List (sort, sortBy, minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Vector.Storable        as VS
import Data.Vector.Storable ((!))

import qualified Data.Vector.Storable.MMap   as MM
import System.Directory
import System.Environment (getArgs)

import qualified Data.Smashy.Map as HM
import Data.Smashy.Types
import Access
import Rank
import Select 
import Shared

import Types
 
main :: IO ()
main = do

    args <- getArgs

    hm <- loadTermMap
    rm <- loadReverseMap
    tree <- loadTree
    
    go hm rm tree ("-i" `elem` args)

go :: HashMap Term TermInfo
   -> HashMap TermId Term
   -> WaveletTree
   -> Bool
   -> IO ()
go hm rm node rept = do
    putStr "Length: "
    print len
    go'

    where
    len = fromIntegral $ node ! 2
    go' = do

        ws <- words <$> getLine

        case ws of

            [] -> return ()

            ["r",term] -> do
                Just (TermInfo _ tid _ _) <- HM.get hm (C8.pack term)
                let r = rank tid len node
                print r
                when rept go'

            ["r",term,strUpper] -> do
                Just (TermInfo _ tid _ _) <- HM.get hm (C8.pack term)
                let r = rank tid (read strUpper :: Int) node
                print r
                when rept go'
                
            ["s",term,strNth] -> do
                Just (TermInfo _ tid _ _) <- HM.get hm (C8.pack term)
                let sel = select tid node (read strNth :: Int)
                print sel
                when rept go'
                
            ["s",term] -> do
                Just (TermInfo _ tid _ _) <- HM.get hm (C8.pack term)
                let sel = selectAll tid node
                print sel
                when rept go'

            ["a",strPos] -> do
                let acc = access node (read strPos :: Int)
                Just term <- HM.get rm acc
                print (acc, term)
                when rept go'

            ["t", term] -> do
                HM.get hm (C8.pack term) >>= print
                when rept go'
                
            ["i", tid] -> do
                HM.get rm (read tid :: TermId) >>= print
                when rept go'

            terms -> do
                searchTerms hm rm len node (map C8.pack terms)
                when rept go'

k1 :: Float
k1 = 1.2 --or 2.0

b :: Float
b = 0.75

searchTerms :: HashMap C8.ByteString TermInfo
            -> HashMap TermId C8.ByteString
            -> Int 
            -> VS.Vector Word64
            -> [C8.ByteString]
            -> IO ()
searchTerms hm rm len node terms = do

    let numDocs = rank 101 (len-1) node
        nonSepWords = fromIntegral $ len - numDocs
        avgDocLength = nonSepWords / fromIntegral numDocs

    termInfos <- mapM (\t -> fromJust <$> HM.get hm t) terms

    let docs = map toDoc (retrieveDocs len node termInfos)        
        scored = -- take 10
                 -- sortBy (flip $ comparing snd) 
                 sortBy (comparing fst)
               . zip docs
               . map (scoreForDoc avgDocLength numDocs termInfos)
               $ docs

    urls <- mapM (HM.get rm . \(Document pos _ ,x) -> (access node (fromIntegral pos+2))) scored
    
    mapM_ print (zip scored (map fromJust urls))
    
    where
    scoreForDoc avgDocLength numDocs termInfos doc =
        sum $ map (scoreTerm doc avgDocLength node numDocs) termInfos
    
--TODO: This pair of lines might be off.  should a doc only start from the content?
toDoc (_,(l,u)) = Document (fromIntegral l) (u-l-1)    
data Document = Document Word32 Int deriving (Show, Eq, Ord)

scoreTerm :: Document -> Float -> WaveletTree -> Int -> TermInfo -> Float
scoreTerm doc@(Document _ docLen) avdoclen node numDocs term = 
    idf' * (tf' * (k1 + 1)) / (tf' + k1 * (1 - b + b * (fromIntegral docLen / avdoclen)))
    where
    idf' = idf numDocs term
        where
        idf :: Int -> TermInfo -> Float
        idf numDocs (TermInfo _ tid _ indocs) = log (x / y)
            where x = fromIntegral numDocs - fromIntegral indocs + 0.5
                  y = fromIntegral indocs + 0.5

    tf' = tf node doc term 
        where
        tf :: WaveletTree -> Document -> TermInfo -> Float
        tf node (Document off len) (TermInfo _ tid _ _) =
            fromIntegral $ rank tid (fromIntegral off + len) node - rank tid (fromIntegral off) node

retrieveDocs :: Int -> WaveletTree -> [TermInfo] -> [(Int,(Int,Int))]
retrieveDocs len node terms = go
                            . map (\(TermInfo _ tid _ _) -> tid)
                            . sortBy (comparing $ \(TermInfo _ _ _ indocs) -> indocs)
                            $ terms
    where
    go :: [TermId] -> [(Int,(Int,Int))]
    go (rareTermId:otherTermIds) = do

        let rarestPositions = selectAll rareTermId node
            relevantDocNums = unrepeat . map (\x -> rank 0 x node) $ rarestPositions 
            docBounds = map (\dn -> (select 0 node (dn-1) ,
                                     select 0 node dn)) relevantDocNums

            documents = zip relevantDocNums docBounds

        filter (pre otherTermIds) documents

        where
        pre :: [Word32] -> (Int,(Int,Int)) -> Bool --Change this from bool to int to get partial matches
        pre otherTermIds (_,(l,u)) = all (\tid -> rank tid u node - rank tid l node > 0) otherTermIds

        unrepeat :: Eq a => [a] -> [a]
        unrepeat [] = []
        unrepeat (x:xs) = x : go x xs
            where
            go _     [] = []
            go p (x:xs)
                | p == x    =     go p xs
                | otherwise = x : go x xs
        