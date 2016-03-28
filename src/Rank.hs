{-# LANGUAGE BangPatterns #-}

module Rank where

import Data.Bits                (testBit)
import Data.Word                (Word32, Word64)
import Data.Vector.Storable     (Vector, (!))

import Shared

rankAll :: Word32 -> Vector Word64 -> Int
rankAll tid dat = rank tid (getNumBits dat 0) dat

rank :: Word32 -> Int -> Vector Word64 -> Int
rank tid bitLimit_ dat =
    rank' bitLimit_ 0 0
    where
    rank' :: Int -> Int -> Int -> Int
    rank' !bitLimit !depth ptr
        | dat ! ptr == 76 = bitLimit
        | dat ! ptr == 78 =
            let bit = testBit tid depth
                layerCount = fastRankSingleLayer dat ptr bitLimit bit            
            in rank' layerCount (depth+1) ((if bit then getRightTree else getLeftTree) dat ptr)