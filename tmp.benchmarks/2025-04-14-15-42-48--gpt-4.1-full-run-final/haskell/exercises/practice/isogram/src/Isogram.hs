module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import qualified Data.Set as Set

isIsogram :: String -> Bool
isIsogram str = go (map toLower str) Set.empty
  where
    go [] _ = True
    go (c:cs) seen
      | not (isLetter c) = go cs seen
      | c `Set.member` seen = False
      | otherwise = go cs (Set.insert c seen)
