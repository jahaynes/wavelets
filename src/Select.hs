{-# LANGUAGE BangPatterns #-}

module Select where

import Data.Bits                 (testBit)
import Data.Word                 (Word32, Word64)
import Data.Vector.Storable      (Vector, unsafeIndex)
import Shared

selectAll :: Word32 -> Vector Word64 -> [Int]
selectAll tid dat = select' 0 0    

    where
    select' :: Int -> Int -> [Int]
    select' depth ptr
        | dat `unsafeIndex` ptr == 76 = [0..fromIntegral $ dat `unsafeIndex` (ptr + 1)-1]
        | dat `unsafeIndex` ptr == 78 = do

            let subs = select'
                           (depth+1)
                           ((if testBit tid depth
                                 then getRightTree
                                 else getLeftTree) dat ptr)

            map (\s -> findNthBitFast (testBit tid depth) s dat ptr) subs        

select :: Word32 -> Vector Word64 -> Int -> Int
select tid dat = select' 0 0

    where
    select' :: Int -> Int -> Int -> Int
    select' depth ptr !nth
        | dat `unsafeIndex` ptr == 76 =
            if nth >= fromIntegral (dat `unsafeIndex` (ptr + 1))
                then error "not enough"
                else nth
        | dat `unsafeIndex` ptr == 78 = do

            let sub = select'
                          (depth+1)
                          ((if testBit tid depth then getRightTree else getLeftTree) dat ptr)
                          nth

            findNthBitFast (testBit tid depth) sub dat ptr
                        

            