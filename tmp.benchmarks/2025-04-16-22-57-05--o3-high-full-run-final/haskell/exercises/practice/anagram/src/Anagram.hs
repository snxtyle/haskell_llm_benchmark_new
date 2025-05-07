module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Determine the list of anagrams of a given word found inside a list
--   of candidate words.
--
--   The comparison is case‑insensitive and a word is **not** considered
--   an anagram of itself (ignoring case).  Each candidate keeps its
--   original casing in the output.
--
--   Examples
--   >>> anagramsFor "master" ["stream", "pigeon", "maters"]
--   ["stream","maters"]
anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    lowerTarget = map toLower target
    sortedTarget = sort lowerTarget

    isAnagram :: String -> Bool
    isAnagram word =
      let lowerWord   = map toLower word
      in  lowerWord /= lowerTarget       -- exclude the word itself
          && sort lowerWord == sortedTarget
