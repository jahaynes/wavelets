{-# LANGUAGE BangPatterns #-}

module Shared where

import Data.Bits                        (popCount, testBit, (.&.))
import Data.Vector.Storable as VS       (Vector, (!), slice, head, tail, drop)
import Data.Word                        (Word64)

divUp :: Integral a => a -> a -> a
divUp a b =
    case a `divMod` b of
        (n,0) -> n
        (n,_) -> n + 1

fastRankSingleLayer :: Vector Word64 -> Int -> Int -> Bool -> Int
fastRankSingleLayer dat ptr bitLimit wantOnes
    | wantOnes = ones
    | otherwise = bitLimit - ones
        where
        ones =
            case bitLimit `divMod` (2048 * 64) of
                (0,           _) -> go 0 bitLimit (getBitBlocks dat ptr)
                (bn, remainBits) -> getRankBlock (bn-1) dat ptr
                                  + go 0 remainBits (VS.drop (bn * 2048) (getBitBlocks dat ptr))           

        go :: Int -> Int -> Vector Word64 -> Int
        go !acc !remaining bitBlocks
            | remaining <= 64 = acc + popCount (((2 ^ remaining) - 1) .&. VS.head bitBlocks)
            | otherwise = go (acc + (popCount . VS.head $ bitBlocks)) (remaining-64) (VS.tail bitBlocks)

findNthBitFast :: Bool -> Int -> Vector Word64 -> Int -> Int
findNthBitFast !wantOne !nth dat ptr =

    let bitBlocks = getBitBlocks dat ptr
        selBlocks = if wantOne then getSel1Blocks dat ptr else getSel0Blocks dat ptr
        (wpn, bitsAfterWpn) = (nth+1) `divMod` (2048 * 64)

    in if wpn > 0
        then
            let totalSkipBits = selBlocks ! (wpn - 1)
                (skipWords, skipBits) = totalSkipBits `divMod` 64
            in go 0 bitBlocks (fromIntegral skipWords) (fromIntegral skipBits) (bitsAfterWpn+1)
        else go 0 bitBlocks 0 0 (nth+1)

    where
    go :: Int -> Vector Word64 -> Int -> Int -> Int -> Int
    go !acc bitBlocks !wix  !bitIx !toFind
        | bitIx == 0 && toFind - acc >= 64 = do
            let acc' = if wantOne then acc + popCount (bitBlocks ! wix) else acc + (64 -  popCount (bitBlocks ! wix))
            go acc' bitBlocks (wix+1) 0 toFind
        | bitIx == 64   = go acc bitBlocks (wix+1) 0 toFind
        | acc == toFind = 64 * wix + bitIx - 1
        | otherwise = do
            let acc' = if wantOne == testBit (bitBlocks ! wix) bitIx then acc + 1 else acc
            go acc' bitBlocks wix (bitIx+1) toFind        

getLeftTree :: Vector Word64 -> Int -> Int
getLeftTree dat ptr =
    ptr + 6 + fromIntegral (numSel0Blocks + numSel1Blocks + numRankBlocks + numBitBlocks)
    where
    numSel0Blocks = dat ! (ptr + 3)
    numSel1Blocks = dat ! (ptr + 4)
    numRankBlocks = dat ! (ptr + 5)
    numBitBlocks = (dat ! (ptr + 2)) `divUp` 64

getRightTree :: Vector Word64 -> Int -> Int
getRightTree dat ptr = ptr + fromIntegral (dat ! (ptr + 1))

getNumBits :: Vector Word64 -> Int -> Int
getNumBits dat ptr = fromIntegral $ dat ! (ptr + 2)

getNumSel0Blocks :: Vector Word64 -> Int -> Int
getNumSel0Blocks dat ptr = fromIntegral $ dat ! (ptr + 3)

getNumSel1Blocks :: Vector Word64 -> Int -> Int
getNumSel1Blocks dat ptr = fromIntegral $ dat ! (ptr + 4)

getNumRankBlocks :: Vector Word64 -> Int -> Int
getNumRankBlocks dat ptr = fromIntegral $ dat ! (ptr + 5)

getNumBitBlocks :: Vector Word64 -> Int -> Int
getNumBitBlocks dat ptr = fromIntegral ((dat ! (ptr + 2)) `divUp` 64)

getRankBlock :: Int -> Vector Word64 -> Int -> Int
getRankBlock n dat ptr =
    fromIntegral (dat ! (n + ptr + 6 + fromIntegral (numSel0Blocks + numSel1Blocks)))
    where
    numSel0Blocks = dat ! (ptr + 3)
    numSel1Blocks = dat ! (ptr + 4)

getSel0Blocks :: Vector Word64 -> Int -> Vector Word64
getSel0Blocks dat ptr =
    slice
        (ptr + 6)
        (fromIntegral numSel0Blocks)
        dat
    where
    numSel0Blocks = dat ! (ptr + 3)

getSel1Blocks :: Vector Word64 -> Int -> Vector Word64
getSel1Blocks dat ptr =
    slice
        (ptr + 6 + fromIntegral numSel0Blocks)
        (fromIntegral numSel1Blocks)
        dat
    where
    numSel0Blocks = dat ! (ptr + 3)
    numSel1Blocks = dat ! (ptr + 4)

getBitBlocks :: Vector Word64 -> Int -> Vector Word64
getBitBlocks dat ptr =
    slice
        (ptr + 6 + fromIntegral (numSel0Blocks + numSel1Blocks + numRankBlocks))
        (fromIntegral numBitBlocks)
        dat
    where
    numBitBlocks = (dat ! (ptr + 2)) `divUp` 64
    numSel0Blocks = dat ! (ptr + 3)
    numSel1Blocks = dat ! (ptr + 4)
    numRankBlocks = dat ! (ptr + 5)