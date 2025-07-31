module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import qualified Data.Set as Set

-- | Determine if a word or phrase is an isogram.
--   Letters are compared case-insensitively.
--   Spaces and hyphens are allowed to appear multiple times and are ignored.
isIsogram :: String -> Bool
isIsogram = go Set.empty
  where
    go _ [] = True
    go seen (c:cs)
      | not (isLetter c) =
          -- Ignore any non-letter characters (spaces, hyphens, punctuation)
          go seen cs
      | lc `Set.member` seen = False
      | otherwise = go (Set.insert lc seen) cs
      where
        lc = toLower c
