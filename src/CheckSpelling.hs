module CheckSpelling where

import Control.Applicative      ((<$>))
import Data.Smashy.Types
import Types
import Filters
import Spelling
import Text.EditDistance
import Data.Maybe (catMaybes, isJust)

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set     as S (toList, fromList)

import qualified Data.Smashy.Map                as HM
import qualified Data.ByteString.Char8          as C8 
import qualified Data.ByteString.Lazy.Char8     as L8

getSpelling :: HashMap Term TermInfo -> HashMap TermId Term -> HashMap Term [TermId] -> L8.ByteString -> IO [Term]
getSpelling hm rm sm input = do
    
    let inputTerms = alphaFilter input
    
    mTermInfos <- mapM (HM.get hm) inputTerms
    processed <- mapM processTermInfo (zip inputTerms mTermInfos)
    return . concat . map (take 1) $ processed

    where
    processTermInfo (term, Nothing) = fetchBetter term Nothing
    processTermInfo (term, Just (TermInfo _ _ _ docFreq))
        | docFreq < 5 = fetchBetter term (Just docFreq)    --docFreq * 1000 < numDocuments
        | otherwise = return [term]

    fetchBetter term mDocFreqToBeat = do

        let unpackedTerm = C8.unpack term

        potentialIds <- S.toList . S.fromList . concat . catMaybes <$> mapM (HM.get sm) (if C8.length term < 12 then maxNDeletes 3 term else [term])
        potentialTerms <- catMaybes <$> mapM (HM.get rm) potentialIds
        mTInfos <- mapM (HM.get hm) potentialTerms

        let results = case mDocFreqToBeat of
                          Nothing -> termsAndValidDocFreqs $ zip potentialTerms mTInfos
                          Just docFreqToBeat -> (term, docFreqToBeat) : (termsAndValidDocFreqs $ zip potentialTerms mTInfos)

            scored = map (\(pTerm,pDocFreq) -> do
                         let dist = levenshteinDistance defaultEditCosts unpackedTerm (C8.unpack pTerm)
                         (pTerm, log (fromIntegral $ 1 + pDocFreq :: Float) / fromIntegral dist)) results

            goodEnough = case mDocFreqToBeat of
                             Nothing -> scored
                             Just _ -> let ((ref, score) : rest) = scored
                                       in (ref, score) : filter (\(_, pScore) -> pScore > score) rest

            sorted = sortBy (flip $ comparing snd) goodEnough

        return $ map fst sorted

        where
        termsAndValidDocFreqs = map (\(pTerm, Just (TermInfo _ _ _ pDocFreq)) -> (pTerm, pDocFreq)) . filter (isJust . snd)
        
            
        
        
