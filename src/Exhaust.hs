{-# LANGUAGE BangPatterns #-}

import Shared
import Types
import qualified Data.Smashy.Map as HM (get, terms)
import Rank (rank, rankAll)
import Access (access)
import Control.Monad (when, unless, forM_, forM)
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as VS (length)
import Data.List.Split                      (chunksOf)
import Control.Concurrent.Async

getNumDocs :: WaveletTree -> Int
getNumDocs = rankAll 0

getCorpusLength :: WaveletTree -> Int
getCorpusLength tree = getNumBits tree 0

main :: IO ()
main = do

    hm <- loadTermMap
    rm <- loadReverseMap
    tree <- loadTree
    counts <- loadCounts

    let numDocs = getNumDocs tree
    putStr "Num docs: " >> print numDocs

    let corpusLength = getCorpusLength tree
    putStr "Corpus length: " >> print corpusLength

    tc <- HM.terms (0 :: Int) (\i _ -> return (i + 1)) hm
    putStr "Num terms: " >> print tc

    let numCounts = VS.length counts
    putStr "Counts length: " >> print numCounts

    -- Dictionary Checks
    (termCount, _) <- HM.terms (0,0) (\(i, prevCount) term -> do

        Just (TermInfo termType tid lastDoc inDocs) <- HM.get hm term

        -- Check the retrieved tid matches our current count
        unless (i == tid) (error $ "Tids didn't line up: " ++ show (i, tid))

        -- Check we get our original term back when going hashMap -> reverseMap
        Just termBack <- HM.get rm tid
        unless (term == termBack) (error $ "Terms did not match: " ++ show (term, termBack))

        -- Check "last doc" is all within range 
        unless (lastDoc < fromIntegral numDocs) (error $ "Lastdoc of term found beyond final doc: " ++ show (lastDoc, numDocs))

        -- TODO: verify lastDoc by ranking doc seps up to last selection
        
        -- TODO: verify rank equals length of selectAll        
        
        -- Check rank equals count (for all but the first)
        let rnk = rankAll tid tree
            thisCount = if termType == BasicTerm then (counts ! fromIntegral i) else prevCount

        when (termType == BasicTerm) $ do
            
            let count = thisCount - prevCount
            unless (rnk == count) $ do
                putStrLn $ "Rank/Counts did not match for tid: " ++ show tid
                putStrLn $ "Counts: " ++ show (prevCount, thisCount, count)
                putStrLn $ "Rank: " ++ show rnk
                error "Failed"

        --Check doc frequency is less than or equal to rank
        unless (inDocs <= fromIntegral rnk) (error $ "Doc Frequency was higher than rank for: " ++ show (term, tid, inDocs, rnk))

        return (i+1, thisCount)) hm

    print "All dictionary checks passed!"

    --Check we have as many terms as we have nodes in the tree
    let leafCount = countLeaves tree
    unless (termCount == fromIntegral leafCount) (error $ "Differing numbers of terms and tree leaves: " ++ show (termCount, leafCount))

    print "Structure/count check passed!"

    --Slow bit, let's parallelise
    let numCores = 8
        rnge = [1..corpusLength]
        subCorpusSize = corpusLength `div` numCores
        chunks = chunksOf subCorpusSize rnge

    forM chunks (async . checkChunk tree) >>= mapM_ wait

    print "Rank/Access check passed!"

checkChunk tree chunk =
    forM_ chunk (\j ->
        let tidAtJ = access tree (j-1) in
        unless (rank tidAtJ j tree == 1 + rank tidAtJ (j-1) tree) (error $ "Access/Rank invariant failed at: " ++ show j))
    
countLeaves :: WaveletTree -> Int
countLeaves tree = countLeaves' 0
    where
    countLeaves' ptr =
        case tree ! ptr of
             76 -> 1
             78 -> countLeaves' (getLeftTree tree ptr) + countLeaves' (getRightTree tree ptr)
             _  -> error "Fell out of tree!"

    
    