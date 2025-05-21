module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Checks if a candidate word is an anagram of the target word.
-- An anagram must have the same letters (case-insensitive) but not be the exact same word (case-insensitive).
isAnagram :: String -> String -> Bool
isAnagram target candidate =
    (normalize target == normalize candidate) &&
    (not (isSameWordCaseInsensitive target candidate))
  where
    -- | Normalizes a string by converting it to lowercase and sorting its characters.
    normalize :: String -> String
    normalize = sort . map toLower

    -- | Checks if two strings are identical when ignoring case.
    isSameWordCaseInsensitive :: String -> String -> Bool
    isSameWordCaseInsensitive s1 s2 = map toLower s1 == map toLower s2

-- | Given a target word and a list of candidate words, returns the subset of candidates
-- that are anagrams of the target word.
anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates =
    filter (isAnagram target) candidates
