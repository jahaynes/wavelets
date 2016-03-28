{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import Types
import Shared

import Control.Applicative ((<$>))

import Data.Maybe (catMaybes)

import qualified Data.ByteString.Char8  as C8

import qualified Data.Smashy.Map        as HM
import qualified Data.Heap              as H

import Data.Vector.Storable as VS       ((!))

import Data.Bits                        (setBit)

--import Data.List                        (partition)

import Access (access)
import Select (select)

import Data.Smashy.Types

import System.Environment (getArgs)

data Entry = Expanded !Int !DocId
           | Unexpanded !Int !Int ![(Int,Int)] !Int !DocId deriving (Eq, Ord, Show)
       
main :: IO ()
main = do

    (a:as) <- getArgs   

    --Load the index files from disk
    (hm, rm, sm, wt, counts, post) <- usualSuspects a

    let ws = map C8.pack as

    azs <- catMaybes <$> mapM (seed hm counts) ws

    let firstEntry = Unexpanded 0 0 azs 0 0

    let docIds = map (\(Expanded _ docId) -> docId) .  take 10 . expand post $ (H.singleton firstEntry)

    let urlIdPoss = map (\d -> 2 + select 0 wt (fromIntegral d - 1)) docIds

    --mapM_ print docIds
    let urlIds = map (access wt) urlIdPoss

    urls <- mapM (HM.get rm) urlIds

    mapM_ print urls


expand :: PostingsWaveletTree -> H.Heap Entry -> [Entry]
expand post heap =

    case H.uncons heap of

        Nothing -> []

        Just (e@(Expanded _ _), heap') -> e : expand post heap'

        Just (Unexpanded score ptr azs h path, heap') ->

            case post ! ptr of
                76 -> expand post (H.insert (Expanded score path) heap')

                78 ->

                    let bits = map (\(a,z) ->      
                            let prevOnes = fastRankSingleLayer post ptr a True
                                prevZeroes = a - prevOnes
                                allOnes = fastRankSingleLayer post ptr z True
                                allZeroes = z - allOnes
                                ones = allOnes - prevOnes
                                zeroes = allZeroes - prevZeroes
                            in (prevOnes,prevZeroes,allOnes,allZeroes,ones,zeroes)) azs

                        leftScore = maximum . map (\(_,_,_,_,_,zeroes)->zeroes) $ bits
                        rightScore = maximum . map (\(_,_,_,_,ones,_)->ones) $ bits

                        leftAzs = concatMap (\(_,prevZeroes,_,allZeroes,_,zeroes) -> do
                                    if zeroes == 0
                                        then []
                                        else [(prevZeroes, allZeroes)]) bits

                        rightAzs = concatMap (\(prevOnes,_,allOnes,_,ones,_) -> do
                                    if ones == 0
                                        then []
                                        else [(prevOnes, allOnes)]) bits

                        lefts = if (length leftAzs < length azs)
                                    then []
                                    else [Unexpanded (-leftScore) (getLeftTree post ptr) leftAzs (h+1) path]

                        rights = if (length rightAzs < length azs)
                                     then []
                                     else [Unexpanded (-rightScore) (getRightTree post ptr) rightAzs (h+1) (setBit path h)]

                    in expand post (foldr H.insert heap' (lefts ++ rights))

seed :: HashMap Term TermInfo -> Counts -> Term -> IO (Maybe (Int,Int))
seed hm counts term = HM.get hm term >>= \mtinfo ->
    case mtinfo of
        Nothing -> return Nothing
        Just (TermInfo _ tid _ _) -> do
            --print ("tid", tid)
            return $
                Just $ (counts ! fromIntegral (tid - 1),
                        counts ! fromIntegral tid)

