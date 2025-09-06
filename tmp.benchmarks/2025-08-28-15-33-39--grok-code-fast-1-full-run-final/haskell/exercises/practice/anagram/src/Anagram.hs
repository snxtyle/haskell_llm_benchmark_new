module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    norm = map toLower
    sortedNorm = sort . norm
    targetNorm = norm target
    targetSorted = sortedNorm target
    isAnagram cand = let candNorm = norm cand
                         candSorted = sortedNorm cand
                     in candSorted == targetSorted && candNorm /= targetNorm
