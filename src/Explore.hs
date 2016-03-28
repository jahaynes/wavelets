{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Data.Word                        (Word32)
import Data.Vector.Storable as VS       ((!))
import Data.Bits                        (setBit)

import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy.Char8     as L8

import Data.List                        (nub, sortBy, groupBy)
import Data.Set as S                    (toList, fromList)
import Data.Ord                         (comparing)
import Data.Maybe                       (fromJust, catMaybes)

import System.Environment               (getArgs)

import qualified Data.Map.Strict        as M
import qualified Data.Smashy.Map        as HM
import Data.Smashy.Types
import Control.Applicative              ((<$>))
import Control.Monad                    (forM, forM_, unless)

import Constants
import Types
import Shared
import Select       (select)
import Rank         (rankAll)
import Access       (access)
import Filters      (alphaFilter)

import Spelling     (maxNDeletes)

import Text.EditDistance    

getTermIdsFromInput :: HashMap Term TermInfo -> Int -> IO [ClassifiedTerm]
getTermIdsFromInput hm numDocs = do
    
    rawQuery <- L8.pack . unwords <$> getArgs
    forM (alphaFilter rawQuery) $ \fterm -> do
        mtid <- HM.get hm fterm
        case mtid of
            Nothing -> return $ Missing fterm
            Just (TermInfo _ tid _ inDocs) -> do
                let docFreq = fromIntegral inDocs
                if docFreq >= div numDocs 2
                    then return $ StopWord fterm
                    else return $ KeyWord fterm tid docFreq

main :: IO ()
main = do

    --Load the index files from disk
    (hm, rm, sm, tree, counts, posttree) <- usualSuspects "."    


    (termsAndTids, stopWords, missingTerms) <- splitClassifieds <$> getTermIdsFromInput hm (rankAll 0 tree)

    let (terms, tids, docFreqs) = unzip3 termsAndTids

    unless (null missingTerms) $ do
        putStr "Not Found: "
        print missingTerms
        putStrLn ""

        presentSpellingSuggestions missingTerms sm rm tree
        
    unless (null stopWords) $ do
        putStr "Stop Words: "
        print stopWords
        putStrLn ""

    let explain = sortBy (comparing snd) $ zip terms docFreqs
    mapM_ print explain
    putStrLn ""

    let scored = getResults 0 9 tids counts posttree docFreqs

    runQuery tree scored hm rm

    where
    --TODO major speedup here by not sorting everything? Just top-k?
    getResults :: Int -> Int -> [TermId] -> Counts -> PostingsWaveletTree -> [DocFrequency] -> [(DocId, Float)]
    getResults lo hi tids counts posttree docFreqs 
            = take (hi-lo+1)
            . drop lo 
            . sortBy (flip $ comparing snd)
            . map scoreDoc 
            $ groupedByDoc
        where
        groupedByDoc :: [[(DocId, TermId, TermFrequency)]]
        groupedByDoc = groupBy (\(a,_,_) (b,_,_) -> a == b) docTermCounts

        docTermCounts :: [DocTermCount]
        docTermCounts = getFreq posttree counts tids

        scoreDoc :: [DocTermCount] -> (DocId, Float)
        scoreDoc dtcs = do
            let scored = map replaceWithScore dtcs
            (fst . head $ scored, sum . map snd $ scored)
            where
            replaceWithScore :: DocTermCount -> (DocId, Float)
            replaceWithScore (docId, tid, count) = do
                let score = fromIntegral count * (fromJust . M.lookup tid $ termScores)
                (docId, score)
        
            termScores :: M.Map TermId Float
            termScores = scoreTerms $ zip tids docFreqs
        
            scoreTerms :: [(TermId, DocFrequency)] -> M.Map TermId Float
            scoreTerms = M.fromList . map (\(tid,docFreq) -> (tid, 1.0 / fromIntegral docFreq))

    presentSpellingSuggestions :: [Term] -> HashMap Term [TermId] -> HashMap TermId Term -> WaveletTree -> IO ()
    presentSpellingSuggestions missingTerms sm rm tree = do
        let maybeMispelled = S.toList . S.fromList . concatMap (maxNDeletes 3) $ missingTerms
        maybeBetterTids <- concat . catMaybes <$> mapM (HM.get sm) maybeMispelled
        suggestionsAndTids <- (\ss -> S.toList . S.fromList . zip (map fromJust ss) $ maybeBetterTids) <$> mapM (HM.get rm) maybeBetterTids
        
        let suggestionsRanksDists =
                map (\(s,t) ->
                   (s,
                    rankAll t tree,
                    levenshteinDistance
                        defaultEditCosts
                        (C8.unpack $ head missingTerms)
                        (C8.unpack s))) suggestionsAndTids

        let adjusted = map (\(x,y,z) -> (x,y,z,log (1 + fromIntegral y) / fromIntegral z)) suggestionsRanksDists
                        
        let sorted = sortBy ( flip $ comparing (\(_,_,_,a) -> (a :: Double)) ) adjusted
        mapM_ print sorted
        
    splitClassifieds :: [ClassifiedTerm] -> ([(Term, TermId, Int)], [Term], [Term])
    splitClassifieds = spl [] [] []
        where
        spl ks ss ms     [] = (ks, ss, ms)
        spl ks ss ms (x:xs) =
            case x of
                KeyWord term tid docFreq -> spl ((term, tid, docFreq):ks) ss ms xs
                StopWord term -> spl ks (term:ss) ms xs
                Missing term -> spl ks ss (term:ms) xs

    runQuery :: WaveletTree -> [(DocId, Float)] -> HashMap Term TermInfo -> HashMap TermId Term -> IO ()
    runQuery tree scored hm rm =

        forM_ (zip scored ([1..] :: [Int])) $ \((docId,score),n) -> do
            let docStart = select 0 tree (fromIntegral docId - 1)
            url <- getUrlFromDocument docStart
            putStr . paddedNum $ n
            putStr ")  "
            putStr . paddedNum $ score
            putStr "   "
            C8.putStrLn url

        where
        getUrlFromDocument :: Int -> IO Term
        getUrlFromDocument docStart = do
            Just (TermInfo MetaKey urlKeyTid _ _) <- HM.get hm keyUrl
            let urlValTid = findUrlTid urlKeyTid docStart
            Just url <- HM.get rm urlValTid
            return url
            where
            findUrlTid urlKeyTid n | access tree n == urlKeyTid = access tree (n + 1)
                                   | otherwise = findUrlTid urlKeyTid (n + 1)

        paddedNum :: Show a => a -> String
        paddedNum n = do
            let sn = show n
            let toPad = 3 - length sn
            replicate toPad ' ' ++ sn

    getFreq :: PostingsWaveletTree -> Counts -> [TermId] -> [DocTermCount] 
    getFreq post counts tids =
        let distinct = nub tids
            as = map (\t -> counts ! fromIntegral (t-1)) distinct
            zs = map (\t -> counts ! fromIntegral t) distinct
        in getFreq' (length distinct) 0 0 0 (zip3 distinct as zs)
        where
        getFreq' :: Int -> Int -> Word32 -> Int -> [(Word32, Int, Int)] -> [DocTermCount]
        getFreq' !noDistinct !depth !acc ptr tazs

            | post ! ptr == 76 = do

                let ret = map (\(t,f) -> (acc,t,f))
                        . filter (\(_,f) -> f > 0)
                        . map (\(t,a,z) -> (t,z-a))
                        $ tazs           

                if length ret == noDistinct then ret else []

            | post ! ptr == 78 = do

                let prepped = map (\(t,a,z) -> do
                                let predZeroes = fastRankSingleLayer post ptr a False
                                    zers = fastRankSingleLayer post ptr z False - predZeroes
                                    predOnes = fastRankSingleLayer post ptr a True
                                    ones = fastRankSingleLayer post ptr z True - predOnes
                                (t, predZeroes, zers, predOnes, ones) ) tazs

                    filtered = filter (\(_, _, zers, _, ones) -> zers > 0 || ones > 0) prepped

                if length filtered == noDistinct
                    then
                        let forLeft =  map (\(t, predZeroes, zers,        _,    _) -> (t, predZeroes, predZeroes + zers)) filtered
                            forRight = map (\(t,          _,    _, predOnes, ones) -> (t,   predOnes,   predOnes + ones)) filtered
                            l = getFreq' noDistinct (depth+1)                acc  (getLeftTree post ptr)  forLeft
                            r = getFreq' noDistinct (depth+1) (setBit acc depth) (getRightTree post ptr) forRight
                        in l ++ r
                    else []

            | otherwise = error "Fell out of tree"