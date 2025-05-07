module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

-- | Normalizes a word by converting it to lowercase and sorting its characters.
--   This allows easy comparison for anagrams.
normalize :: String -> String
normalize = sort . map toLower

-- | Checks if `candidate` is an anagram of `target`.
--   Comparison is case-insensitive.
--   A word is not considered an anagram of itself.
isAnagram :: String -> String -> Bool
isAnagram target candidate =
    let lowerTarget = map toLower target
        lowerCandidate = map toLower candidate
    in lowerTarget /= lowerCandidate &&  -- Ensure it's not the same word (case-insensitive)
       normalize target == normalize candidate -- Check if letters match (case-insensitive)

-- | Given a target word and a list of candidate words, returns the subset of
--   candidates that are anagrams of the target.
anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss
