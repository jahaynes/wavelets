{-# LANGUAGE OverloadedStrings #-}

module Filters where

import WarcFile

import Data.Char       (isAlpha, toLower)
import Data.Maybe      (fromJust)
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy.Char8     as L8

import Constants
import Types

alphaFilter :: L8.ByteString -> [C8.ByteString]
alphaFilter = map L8.toStrict
            . filter (not . L8.null)
            . L8.words
            . L8.map (\x -> if isAlpha x then toLower x else ' ')

warcFilter :: L8.ByteString -> [(TermType, C8.ByteString)]
warcFilter f = concatMap readWarcEntry (fromBytes f)
    where
    readWarcEntry e =
        let url = L8.toStrict . fromJust . getField "WARC-Record-ID:" $ e
            webdata = alphaFilter . toBytes WebData Uncompressed $ e
        in (DocSep, docsep)
            : (MetaKey, keyUrl) : (MetaVal, url)
            : (map (\t -> (BasicTerm, t)) webdata)