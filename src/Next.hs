import Control.Applicative                          ((<$>))
import Data.List                                    (sortBy, nub)
import Data.Maybe                                   (catMaybes)
import Data.Ord                                     (comparing)
import Data.Vector.Storable.MMap                    (unsafeMMapVector)
import System.Environment                           (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L8   (pack)
import qualified Data.Map as M                      ((!), fromListWith)
import qualified Data.Smashy.Map as HM              (open, get)
import Access                                       (access)
import Data.Smashy.Types                            (HashMap, Location (Disk))
import Filters                                      (alphaFilter)
import Rank                                         (rankAll)
import Select                                       (selectAll)
import Types                                        

getTermIdsFromInput :: IO [TermId]
getTermIdsFromInput = do
    hm <- loadTermMap
    rawQuery <- alphaFilter . L8.pack . unwords <$> getArgs
    map (\(TermInfo _ i _ _) -> i) . catMaybes <$> mapM (HM.get hm) rawQuery

main :: IO ()
main = do

    tree <- loadTree
    tids <- getTermIdsFromInput
    rm <- loadReverseMap

    go tree rm tids >>= mapM_ print

go tree rm tids = do

    let n_tids = nub tids

    --Create a frequency map of the termIds (use deduped tids for speed - only here)
    let freqs = M.fromListWith (+) $ zip n_tids (map (`rankAll` tree) n_tids)

    --List of tuples (order in query, termId, freq) 
        naturalOrder = zip3 [(1::Int)..] tids (map ((M.!) freqs) tids)

    --Ordered list from least frequent to most frequent
        ascending = sortBy (comparing (\(_, _, freq) -> freq)) naturalOrder 

    --Capture the rarest 'ordinal' term
        rarestOrdinal = (\(o, _, _) -> o) . head $ ascending

    --Restate all other 'ordinals' in terms of the rarest order
        offsetToRarest = map (\(o, tid, freq) -> (o - rarestOrdinal, tid, freq)) ascending

    --Now if we have ordinals [-2, -1, 0, 1, 2, 3], then we want the likely ordinal 4s
        ordinalToFind = 1 + (maximum . map (\(o,_,_) -> o) $ offsetToRarest)

    --Now perform the select and bring back the positions in the corpus of 
        positionsOfFound = getNextPositions tree ordinalToFind offsetToRarest

    --Access those positions to get their termIds
        termIdsFound = map (access tree) positionsOfFound

    --Might want to do a frequency sort thingy here

    --(Reverse) lookup those termIds to get back to actual terms
    catMaybes <$> mapM (HM.get rm) termIdsFound

{- Given a list of term tuples, find the positions that are
   offset by 'ordinal' after the end of in-order occurrences
   of these tuples -}
getNextPositions :: WaveletTree -> Int -> [(Int, TermId, Int)] -> [Int]
getNextPositions    _             _                 [] = []
getNextPositions tree ordinalToFind ((_,rareTid,_):as) =
    go (take 10 $ selectAll rareTid tree)
    where
    go     [] = []
    go (x:xs) =
        if all (\(o,tid,_) -> tid == access tree (x+o)) as
            then (x+ordinalToFind) : go xs
            else                     go xs
