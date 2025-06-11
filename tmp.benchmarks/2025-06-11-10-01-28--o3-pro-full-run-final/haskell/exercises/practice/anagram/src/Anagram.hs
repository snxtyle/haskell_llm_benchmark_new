module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Given a target word and a list of candidate words, return the subset
--   of candidates that are anagrams of the target.
--
--   Rules:
--   • Comparison is case-insensitive.
--   • A word is never considered an anagram of itself (ignoring case).
--   • Order of the output follows the order of the input candidates.
anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    normalize :: String -> String
    normalize = map toLower

    targetNorm   = normalize target
    targetSorted = sort targetNorm

    isAnagram :: String -> Bool
    isAnagram w =
      let wNorm   = normalize w
      in  wNorm /= targetNorm         -- not the same word (ignoring case)
          && sort wNorm == targetSorted
