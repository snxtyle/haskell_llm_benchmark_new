module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import qualified Data.Set as Set

-- | Determine whether a word or phrase is an isogram.
--
--   An isogram is a string in which no letter repeats.  Letter
--   comparison is case–insensitive, and non‑letter characters
--   (such as spaces or hyphens) are ignored.
--
--   Examples:
--
--   >>> isIsogram "lumberjacks"
--   True
--   >>> isIsogram "background"
--   True
--   >>> isIsogram "six-year-old"
--   True
--   >>> isIsogram "isograms"
--   False
--
isIsogram :: String -> Bool
isIsogram = go Set.empty
  where
    go :: Set.Set Char -> String -> Bool
    go _    []     = True
    go seen (c:cs)
      | not (isLetter c)        = go seen cs          -- ignore non‑letters
      | lc `Set.member` seen    = False               -- duplicate letter found
      | otherwise               = go (Set.insert lc seen) cs
      where
        lc = toLower c
