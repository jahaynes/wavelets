{-# LANGUAGE BangPatterns, ScopedTypeVariables #-} 

import Data.Vector.Storable  ((!))            
import qualified Data.Vector.Storable                   as VS
import qualified Data.Vector.Storable.Mutable           as VM
import qualified Data.Vector.Storable.MMap              as MM
import Data.Word as W
import qualified Foreign.ForeignPtr                     as FP
import Data.Bits (testBit, setBit, popCount)
import Control.Monad.Primitive (PrimState, PrimMonad)
import qualified System.Posix.Files                     as PF
import System.Directory                 (removeFile)
import System.Environment       (getArgs)
import Constants

main :: IO ()
main = do
    
    args <- getArgs
    
    let (fileData, outName) =
            case args of
                [] -> ("./outfile", "./tree")
                [f] -> (f, f ++ "_tree")
    
    vm <- MM.unsafeMMapMVector fileData MM.WriteCopy Nothing :: IO (VM.IOVector Word32)
    buildNode outName vm
    unmapData vm

buildNode :: FilePath -> VM.IOVector Word32 -> IO ()
buildNode outputName vm_outer = do

    tree <- MM.unsafeMMapMVector outputName MM.ReadWriteEx (Just (0, 158561034)) :: IO (VM.IOVector Word64) 
    elems <- buildSubNode 0 vm_outer tree 0
    unmapData tree
    PF.setFileSize outputName (fromIntegral (elems * 8))

    putStr "elems processed?: "
    print elems
  
    where
    buildSubNode depth vm tree treep = do

        let f = (`testBit` depth)
            len = VM.length vm
            (filled64blocks, remainder) = len `divMod` 64
            bitBufLenBytes = if remainder == 0 then filled64blocks else filled64blocks + 1

        (leftElemCount, allSame) <- rankFalse f vm
        let rightElemCount = len - leftElemCount

        --putStr "Left element count: " >> print leftElemCount
        --putStr "Right element count: " >> print rightElemCount

        if allSame /= Differing
            then do

                -- Write out the leaf node
                VM.write tree treep nodeL
                VM.write tree (treep+1) (fromIntegral len)
                return (treep + 2)

            else do

                let totalNodeSizeElems = 6                                  --Count blocks
                                       + leftElemCount  `div` (2048 * 64)   --Sel0 blocks
                                       + rightElemCount `div` (2048 * 64)   --Sel1 blocks
                                       + len            `div` (2048 * 64)   --Rank blocks
                                       + len            `divUp` 64          --Actual bits 

                -- Split into left, right, and bit buffers
               
                withMaybeBuffer "./bbuf" bitBufLenBytes $ \bBuf -> do
                    withMaybeBuffer "./lbuf" leftElemCount $ \lBuf -> 
                        withMaybeBuffer "./rbuf" rightElemCount $ \rBuf ->
                            inPlacePartitionWithBuffers f vm len leftElemCount lBuf rBuf bBuf

                    {- Perhaps bbuf should be unmapped and remapped as immutable here
                       to save on the freeze later on -}
                            
                    -- Turn bit buffer into node structure

                    withMaybeBuffer "./node" totalNodeSizeElems $ \node -> do
                        bitBufferToNode
                            bBuf
                            len
                            leftElemCount
                            rightElemCount
                            node
                        
                        -- Then copy it into the tree
                        
                        let tree_slice = VM.slice treep totalNodeSizeElems tree
                        VM.copy tree_slice node

                let (left_vm, right_vm) = VM.splitAt leftElemCount vm

                treep' <- buildSubNode (depth+1) left_vm tree (treep + totalNodeSizeElems)

                --Write where the right side is into [L], but relative
                VM.write tree (treep+1) (fromIntegral (treep' - treep))

                buildSubNode (depth+1) right_vm tree treep'

        where
        --Turns a bit buffer into a proper node structure
        bitBufferToNode :: VM.IOVector Word64 
                        -> Int
                        -> Int
                        -> Int
                        -> VM.IOVector Word64 
                        -> IO ()

        bitBufferToNode bitBuf !bitLen leftBits rightBits node = do

            let numBits       = fromIntegral bitLen
                numSel0Blocks = leftBits  `div` (2048 * 64)
                numSel1Blocks = rightBits `div` (2048 * 64)
                numRankBlocks = bitLen    `div` (2048 * 64)

            VM.write node 0 nodeW       -- N
            VM.write node 1 nodeR       -- R
            VM.write node 2 numBits
            VM.write node 3 (fromIntegral numSel0Blocks)
            VM.write node 4 (fromIntegral numSel1Blocks)
            VM.write node 5 (fromIntegral numRankBlocks)

            let sel0At = 6
                sel1At = 6 + numSel0Blocks
                rankAt = sel1At + numSel1Blocks
                bitsAt = rankAt + numRankBlocks

            bs <- VS.freeze bitBuf
            let sel0Blocks = zip [sel0At..] $ groupedZeroes bs bitLen
                sel1Blocks = zip [sel1At..] $ groupedOnes bs bitLen
                rankBlocks = zip [rankAt..] $ ranksFromPackedBits bs bitLen
                bitBlocks  = zip [bitsAt..] $ VS.toList bs

            mapM_ (uncurry $ VM.write node) sel0Blocks
            mapM_ (uncurry $ VM.write node) sel1Blocks
            mapM_ (uncurry $ VM.write node) rankBlocks
            mapM_ (uncurry $ VM.write node) bitBlocks

ranksFromPackedBits :: VS.Vector Word64 -> Int -> [Word64]
ranksFromPackedBits dat = go 0 0 0
    where
    go :: Int -> Word64 -> Int -> Int -> [Word64]
    go !block !prev !ix !remaining
        | remaining < 64 = []
        | block == 2047  = curr : go 0 curr (ix+1) (remaining-64)
        | otherwise      = go (block+1) curr (ix+1) (remaining-64)
            where curr = prev + fromIntegral (popCount (dat ! ix))

groupedZeroes :: VS.Vector Word64 -> Int -> [Word64]
groupedZeroes dat bitLen = chunkByLast $ positionsFromPackedBits False dat bitLen

groupedOnes :: VS.Vector Word64 -> Int -> [Word64]
groupedOnes dat bitLen = chunkByLast $ positionsFromPackedBits True dat bitLen
    
chunkByLast :: [a] -> [a]
chunkByLast = init . go
    where
    go [] = []
    go xs =
        let (some,rest) = splitAt (2048 * 64) xs
        in last some : go rest    

positionsFromPackedBits :: Bool -> VS.Vector Word64 -> Int -> [Word64]
positionsFromPackedBits wantOnes dat = go 0 0 0
    where
    go !pos !wi !bi !remaining
        | bi == 64                          =       go pos (wi+1) 0 remaining
        | remaining == 0                    = []
        | wantOnes == testBit (dat ! wi) bi = pos : go (pos+1) wi (bi+1) (remaining-1)
        | otherwise                         =       go (pos+1) wi (bi+1) (remaining-1)

inPlacePartitionWithBuffers :: (Word32 -> Bool)
                            -> VM.IOVector Word32
                            -> Int
                            -> Int
                            -> VM.IOVector Word32
                            -> VM.IOVector Word32
                            -> VM.IOVector Word64
                            -> IO ()
inPlacePartitionWithBuffers f vm len sep lBuf rBuf bBuf = do

    let (lDest,rDest) = VM.splitAt sep vm
    go
    VM.copy lDest lBuf
    VM.copy rDest rBuf

    where
    go = go' 0 0 0 0 0 0
        where
        go' :: Int -> Int -> Int -> Int -> Word64 -> Int -> IO ()
        go' r i j k w b
            | r == len = VM.write bBuf k w
            | b == 64 = do
                VM.write bBuf k w
                go' r i j (k+1) 0 0
            | otherwise = do
                x <- VM.read vm r
                if f x
                    then do
                        VM.write rBuf j x
                        go' (r+1) i (j+1) k (setBit w b) (b+1)
                    else do
                        VM.write lBuf i x
                        go' (r+1) (i+1) j k w (b+1)

data AllSame x = Unknown | AllThis x | Differing deriving (Eq, Show)

rankFalse :: (Eq a, VM.Storable a, PrimMonad m) => (a -> Bool) -> VM.MVector (PrimState m) a -> m (Int, AllSame a)
rankFalse f vm = rankFalse' (VM.length vm) Unknown 0 0
    where
    rankFalse' !len !allSame !c !i
        | i == len  = return (c, allSame)
        | otherwise = do
            x <- VM.read vm i
            
            let allSame' = case allSame of
                               Differing -> Differing
                               Unknown -> AllThis x
                               AllThis y -> if x == y then AllThis y else Differing
            
            if f x
                then rankFalse' len allSame'    c  (i+1)
                else rankFalse' len allSame' (c+1) (i+1)
    
withMaybeBuffer :: (VM.Storable a) => FilePath -> Int -> (VM.MVector (PrimState IO) a -> IO ()) -> IO ()
withMaybeBuffer fp len f
    | len < 1048576 = VM.new len >>= \buf -> f buf >> unmapData buf
    | otherwise     = do
        buf <- MM.unsafeMMapMVector fp MM.ReadWriteEx (Just (0, len))
        f buf
        unmapData buf
        --removeFile fp

unmapData :: (VM.Storable a) => VM.IOVector a -> IO ()
unmapData = (\(ptr,_,_) -> FP.finalizeForeignPtr (FP.castForeignPtr ptr)) . VM.unsafeToForeignPtr 
    
    
divUp a b = case a `divMod` b of
                (n, 0) -> n
                (n, _) -> n + 1