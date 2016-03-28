{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Smashy.Map        as HM   (open, new, close, get, storeOne, terms)
import Data.Smashy.Types                        (HashMap)
import Types                                    
import Control.Monad                            (foldM)
import qualified Data.ByteString.Char8  as C8   (length)

import Spelling

main :: IO ()
main = do
    hm <- loadTermMap
    empty <- createSpellingMap
    alt' <- HM.terms empty (\alt t -> do
        mt <- HM.get hm t
        case mt of
            Just (TermInfo BasicTerm tid _ _) ->
                if C8.length t < 16
                    then foldM (getPut tid) alt (maxNDeletes 3 t)
                    else return alt
            _ -> return alt) hm
    HM.close alt'
    HM.close hm
    where
    getPut :: TermId -> HashMap Term [TermId] -> Term -> IO (HashMap Term [TermId])
    getPut !tid !hm !t = do
        mtids <- HM.get hm t
        case mtids of
            Nothing -> HM.storeOne hm (t, [tid])
            Just prevTids -> HM.storeOne hm (t, tid:prevTids)
