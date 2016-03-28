{-# LANGUAGE BangPatterns #-}

import qualified Data.Smashy.Map                        as HM   (open, get, terms)
import Data.Smashy.Types                                        (HashMap, Location (Disk))
import Data.Vector.Storable  (Vector, (!))            
import qualified Data.Vector.Storable                   as VS   (length)
import qualified Data.Vector.Storable.Mutable           as VM   
import qualified Data.Vector.Storable.MMap              as MM
import Data.Word as W                                           (Word32, Word64)
import Control.Monad.Primitive                                  (PrimState, PrimMonad)
import System.Posix.Files                                       (setFileSize)
import Foreign.ForeignPtr                                       (castForeignPtr, finalizeForeignPtr)

import Rank
import Access
import Select

import Types       

main :: IO ()
main = makePost "tree" "hmdir" "counts" "postings" "outfile"

makePost :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
makePost treePath hmPath countsPath postingsPath outfilePath = do
    
    tree <- loadTree    
    hm <- loadTermMap
    rm <- loadReverseMap 
      
    (postSz, counts) <- prepareCounts hm rm tree 
    preparePosts postSz counts

    print "Bleh"
    
    where
   
        
    prepareCounts :: HashMap Term TermInfo-> HashMap TermId Term -> WaveletTree -> IO (Int, VM.IOVector Int)
    prepareCounts hm rm tree = do

        allTermCount <- HM.terms (0::Int) (\acc _ -> return (acc+1)) hm
    
        countFile <- MM.unsafeMMapMVector countsPath MM.ReadWriteEx (Just (0, allTermCount)) >>= \f -> VM.set f (0 :: Int) >> return f

        acc <- HM.terms (0::Int) (\acc term -> do
                   mtinfo <- HM.get hm term
                   case mtinfo of
                       Just (TermInfo BasicTerm termId _ _) -> do
                           VM.write countFile (fromIntegral termId) acc
                           return (acc + rankAll termId tree)
                       _ -> return acc) hm

        return (acc, countFile)

    preparePosts :: Int -> VM.IOVector Int -> IO ()
    preparePosts postSz counts = do

        postings <- MM.unsafeMMapMVector postingsPath MM.ReadWriteEx (Just (0, postSz)) :: IO (VM.IOVector Word32)
        VM.set postings 0

        outfile <- getOutfile
        biggest <- go 0 postings outfile 0 0 (VS.length outfile)

        unmapData postings
        setFileSize postingsPath (fromIntegral ((biggest+1) * 4))

        where
        getOutfile :: IO (Vector Word32)
        getOutfile = MM.unsafeMMapVector outfilePath Nothing

        unmapData :: (VM.Storable a) => VM.IOVector a -> IO ()
        unmapData = (\(ptr,_,_) -> finalizeForeignPtr (castForeignPtr ptr)) . VM.unsafeToForeignPtr                    

        go :: Int -> VM.IOVector Word32 -> Vector Word32 -> Word32 -> Int -> Int -> IO Int
        go !biggestPlaceWritten postings outfile !docNo i j
            | i >= j = return biggestPlaceWritten
            | otherwise = do
                let tid = outfile ! i
                if tid == 0
                    then go biggestPlaceWritten postings outfile (docNo+1) (i+2) j
                    else do
                        placeToWrite <- VM.read counts (fromIntegral tid)
                        VM.write postings placeToWrite docNo
                        VM.write counts (fromIntegral tid) (placeToWrite + 1)
                        go (max placeToWrite biggestPlaceWritten) postings outfile docNo (i+1) j
