{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import Types
import Shared

import qualified Data.ByteString.Char8  as C8

import qualified Data.Smashy.Map        as HM
import qualified Data.Heap              as H

import Data.Vector.Storable as VS       ((!))

import Data.Bits                        (setBit)

import Data.List                        (partition)

import Data.Smashy.Types

import System.Environment (getArgs)

data Entry = Expanded Int DocId
           | Unexpanded Int Int Int Int Int DocId deriving (Eq, Ord, Show)

main :: IO ()
main = do

    --Load the index files from disk
    (hm, _, _, _, counts, post) <- usualSuspects "/home/john/ltu"    

    ws <- fmap (map C8.pack) getArgs

    mapM_ (\w -> do
        entry <- getSeed hm counts w

        print ("Start entry", entry)

        let done = fmap (expand post) entry
        C8.putStr w
        C8.putStrLn ": " 
        print done) ws

getSeed :: HashMap Term TermInfo -> Counts -> Term -> IO (Maybe (H.Heap Entry))
getSeed hm counts term = do
    mtinfo <- HM.get hm term
    case mtinfo of
        Nothing -> return Nothing
        Just (TermInfo _ tid _ _) -> do
            print ("tid", tid)
            let a = counts ! fromIntegral (tid - 1)
                z = counts ! fromIntegral tid
            return . Just $ H.singleton (Unexpanded (-(z-a)) 0 a z 0 0)

expand :: PostingsWaveletTree -> H.Heap Entry -> H.Heap Entry
expand post = go H.empty

    where
    go :: H.Heap Entry -> H.Heap Entry -> H.Heap Entry
    go done heap
       | H.size done >= 100 = done
       | otherwise =
           case H.uncons heap of
           Nothing -> done -- print ("Went through everything, found num of docs: ", H.size done) 
           Just (Unexpanded _ ptr a z h path, rest) ->

               let children =
                       case post ! ptr of
                           76 -> [Expanded (-(z-a)) path] --just for reverse order
                           78 ->
                               let prevOnes = fastRankSingleLayer post ptr a True
                                   prevZeroes = a - prevOnes
                                   allOnes = fastRankSingleLayer post ptr z True
                                   allZeroes = z - allOnes
                                   ones = allOnes - prevOnes
                                   zeroes = allZeroes - prevZeroes

                                   lefts = if zeroes == 0 then [] else [Unexpanded (-zeroes) (getLeftTree post ptr) prevZeroes allZeroes (h+1) path]
                                   rights = if ones == 0 then [] else [Unexpanded (-ones) (getRightTree post ptr) prevOnes allOnes (h+1) (setBit path h)]
                               in lefts ++ rights

                   (found, toAdd) = partition (\x -> case x of
                                                        Expanded _ _ -> True
                                                        _ -> False) children

                   heap' = rest `H.union` H.fromList toAdd
                   done' = done `H.union` H.fromList found

               in go done' heap'

