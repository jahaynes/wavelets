{-# LANGUAGE OverloadedStrings #-}

module Constants where

import Data.Char (ord)
import Data.Word (Word64)
import Data.ByteString.Char8 (ByteString)

nodeW :: Word64
nodeW = fromIntegral . ord $ 'N'

nodeR :: Word64
nodeR = fromIntegral . ord $ 'R'

nodeL :: Word64
nodeL = fromIntegral . ord $ 'L'

docsep :: ByteString 
docsep = "$"

keyUrl :: ByteString 
keyUrl = "_url"


