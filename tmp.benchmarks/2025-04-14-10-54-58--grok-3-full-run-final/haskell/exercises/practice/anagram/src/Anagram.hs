module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    -- Convert target to lowercase and sort its characters for comparison
    normalizedTarget = sort $ map toLower target
    
    -- Check if a candidate is an anagram of target
    isAnagram candidate = 
      let normalizedCandidate = sort $ map toLower candidate
      in normalizedCandidate == normalizedTarget && 
         map toLower candidate /= map toLower target
