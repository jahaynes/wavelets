{-# LANGUAGE BangPatterns #-}

module Access where

import Data.Bits                (setBit, testBit)
import Data.Word                (Word32, Word64)
import Data.Vector.Storable     (Vector, (!))

import Shared

access :: Vector Word64 -> Int -> Word32       
access dat pos_ = access' 0 pos_ 0 0
    where
    access' :: Word32 -> Int -> Int -> Int -> Word32   
    access' !acc !pos !depth !ptr
        | depth >= 32     = error "too deep"
        | dat ! ptr == 76 = acc
        | dat ! ptr == 78 =
            let (wo, bo) = pos `divMod` 64
                bit = testBit (getBitBlocks dat ptr ! wo) bo
                isNth = fastRankSingleLayer dat ptr pos bit
            in if bit
                then access' (setBit acc depth) isNth (depth+1) (getRightTree dat ptr)
                else access'         acc        isNth (depth+1)  (getLeftTree dat ptr)
{-
accessRange :: Vector Word64 -> Int -> Int -> IO () -- [Word32]
accessRange dat posLower_ posUpper_ = access' 0 posLower_ posUpper_ 0 0
    where
    access' acc posLower posUpper depth ptr
        | depth >= 32 = error "too deep"
        | dat ! ptr == 76 = print ""
        | otherwise = do

            let (zeroes, ones) = do
                let o = fastRankSingleLayer dat ptr posUpper True
                      - fastRankSingleLayer dat ptr posLower True 
                    z = posUpper - posLower - o
                (z, o)

            print (zeroes, ones)
            
            let (wol, bol) = posLower `divMod` 64
                (wou, bou) = posUpper `divMod` 64
            print (wol, bol, wou, bou)
-}