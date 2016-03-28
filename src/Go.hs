import qualified Data.Smashy.Map                    as HM
import Data.Smashy.Types
import qualified Data.Vector.Storable               as VS
import qualified Data.Vector.Storable.Mutable       as VM
import Data.Vector.Storable.ByteString
import qualified Data.ByteString.Char8              as C8
import qualified Data.ByteString.Lazy.Char8         as L8
import Data.Word (Word32)
import Filters
import Types

data ChunkState = ChunkState DocId TermId (HashMap Term TermInfo) [(TermType, Term)]

chunkSize :: Int
chunkSize = 4096

main :: IO ()
main = do
    hm <- HM.new (Disk "hmdir") :: IO (HashMap Term TermInfo)
    filtered <- fmap warcFilter L8.getContents
    hm' <- go hm filtered
    HM.close hm'

    where

    go :: HashMap Term TermInfo -> [(TermType, Term)] -> IO (HashMap Term TermInfo)
    go = go' 0 0
        where
        go' :: DocId -> TermId -> HashMap Term TermInfo -> [(TermType, Term)] -> IO (HashMap Term TermInfo)            
        go'     _   _ hm [] = return hm
        go' docId nid hm ws = do
            (vm, ChunkState docId' nid' hm' ws') <- nextN docId nid hm ws
            VS.unsafeFreeze vm >>= C8.putStr . vectorToByteString
            go' docId' nid' hm' ws'

            where
            nextN :: DocId
                  -> TermId
                  -> HashMap Term TermInfo
                  -> [(TermType, Term)]
                  -> IO (VM.IOVector Word32, ChunkState)
            nextN docId nid hm ws = do
                let (some, rest) = splitAt chunkSize ws
                vm <- VM.new (length some) :: IO (VM.IOVector Word32)
                (vm', hm', docId', nid') <- go docId 0 nid vm hm some
                return (vm', ChunkState docId' nid' hm' rest)
                where
                go docId _ nid vm hm     [] = return (vm, hm, docId, nid)
                go docId i nid vm hm ((tt, w):ws) = do
                    
                    let docId' = if w == C8.pack "$" then docId + 1 else docId
                    
                    mtid <- HM.get hm w
                    case mtid of
                        Just (TermInfo termType tid lastDoc inDocs) -> do
                            VM.write vm i tid

                            let inDocs' = if docId' == lastDoc then inDocs else inDocs + 1
                            hm' <- HM.storeOne hm (w, TermInfo termType tid docId' inDocs')
                            go docId' (i+1) nid vm hm' ws
                        Nothing -> do
                            hm' <- HM.storeOne hm (w, TermInfo tt nid docId 1)
                            VM.write vm i nid
                            go docId (i+1) (nid+1) vm hm' ws
