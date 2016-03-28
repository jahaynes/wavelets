module Spelling where

import Types
import qualified Data.ByteString.Char8  as C8
import qualified Data.HashSet           as S    (fromList, toList)

maxNDeletes :: Int -> Term -> [Term]
maxNDeletes n_ str_ = S.toList . S.fromList . filter (\x -> C8.length x >= 4) $ go C8.empty n_ str_
    where
    go kept 0 str = [C8.append kept str]
    go kept n str
        | C8.null str = [kept]
        | otherwise = go (C8.snoc kept (C8.head str)) n (C8.tail str) ++ go kept (n-1) (C8.tail str)    


