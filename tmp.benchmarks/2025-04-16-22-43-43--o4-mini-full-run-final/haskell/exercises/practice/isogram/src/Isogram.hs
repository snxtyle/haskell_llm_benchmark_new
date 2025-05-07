module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)
import qualified Data.Set as Set

isIsogram :: String -> Bool
isIsogram str = go str Set.empty
  where
    go [] _ = True
    go (c:cs) seen
      | not (isAlpha c)    = go cs seen
      | Set.member lc seen = False
      | otherwise          = go cs (Set.insert lc seen)
      where
        lc = toLower c
