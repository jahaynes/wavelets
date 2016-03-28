{-# LANGUAGE DeriveGeneric #-}

module Types where

import qualified Data.ByteString.Char8                  as C8
import GHC.Generics (Generic)
import qualified Data.Vector.Storable                   as VS
import Data.Serialize
import Data.Word (Word32, Word64)

import Data.Vector.Storable.MMap        (unsafeMMapVector)

import qualified Data.Smashy.Map        as HM
import Data.Smashy.Types

data TermInfo = TermInfo TermType TermId LastDoc InDocs deriving (Show, Generic)
type Term = C8.ByteString
type DocId = Word32
type LastDoc = DocId
type InDocs = Word32

type TermId = Word32

type WaveletTree = VS.Vector Word64

data TermType = BasicTerm
              | DocSep
              | MetaKey 
              | MetaVal deriving (Show, Eq, Generic)

type PostingsWaveletTree = VS.Vector Word64
type Counts = VS.Vector Int

instance Serialize TermInfo
instance Serialize TermType

type DocFrequency = Int

type TermFrequency = Int

type DocTermCount = (DocId, TermId, TermFrequency)

type ReduceFunc = [DocTermCount] -> [DocTermCount]

data ClassifiedTerm = KeyWord Term TermId DocFrequency
                    | StopWord Term
                    | Missing Term

loadTermMap :: IO (HashMap Term TermInfo)
loadTermMap = HM.open (Disk "./hmdir") 

loadReverseMap :: IO (HashMap TermId Term)
loadReverseMap = HM.open (Disk "./rmdir")

createReverseMap :: IO (HashMap TermId Term)
createReverseMap = HM.new (Disk "./rmdir")

createSpellingMap :: IO (HashMap Term [TermId])
createSpellingMap = HM.new (Disk "./spelling") 

loadSpellingMap :: IO (HashMap Term [TermId])
loadSpellingMap = HM.open (Disk "./spelling") 

loadCounts :: IO (VS.Vector Int)
loadCounts = unsafeMMapVector "./counts" Nothing

loadTree :: IO WaveletTree
loadTree = unsafeMMapVector "./tree" Nothing

loadPostingsTree :: IO PostingsWaveletTree
loadPostingsTree = unsafeMMapVector "./postings_tree" Nothing

usualSuspects :: FilePath -> IO (HashMap Term TermInfo, HashMap TermId Term, HashMap Term [TermId], WaveletTree, Counts, PostingsWaveletTree)
usualSuspects fp = do

    hm <- HM.open (Disk $ fp ++ "/hmdir") 
    rm <- HM.open (Disk $ fp ++ "/rmdir")
    sm <- HM.open (Disk $ fp ++ "/spelling")

    tree <- unsafeMMapVector (fp ++ "/tree") Nothing
    counts <- unsafeMMapVector (fp ++ "/counts") Nothing
    posttree <- unsafeMMapVector (fp ++ "/postings_tree") Nothing

    return (hm, rm, sm, tree, counts, posttree)
