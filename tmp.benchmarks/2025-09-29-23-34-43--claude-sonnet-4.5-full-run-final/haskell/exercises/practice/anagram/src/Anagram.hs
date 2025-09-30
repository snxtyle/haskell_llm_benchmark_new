module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    normalizedTarget = map toLower xs
    sortedTarget = sort normalizedTarget
    
    isAnagram candidate =
      let normalizedCandidate = map toLower candidate
          sortedCandidate = sort normalizedCandidate
      in sortedCandidate == sortedTarget && normalizedCandidate /= normalizedTarget
