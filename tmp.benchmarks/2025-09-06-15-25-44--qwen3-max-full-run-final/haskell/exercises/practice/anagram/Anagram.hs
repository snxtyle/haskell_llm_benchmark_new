module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    targetLower = map toLower target
    targetSorted = sort targetLower
    
    isAnagram candidate = 
      let candidateLower = map toLower candidate
          candidateSorted = sort candidateLower
      in candidateSorted == targetSorted && candidateLower /= targetLower
