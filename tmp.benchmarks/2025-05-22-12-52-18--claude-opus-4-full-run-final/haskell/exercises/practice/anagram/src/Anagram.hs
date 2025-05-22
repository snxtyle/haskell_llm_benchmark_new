module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter (isAnagramOf target) candidates

isAnagramOf :: String -> String -> Bool
isAnagramOf target candidate =
  lowerTarget /= lowerCandidate && sortedTarget == sortedCandidate
  where
    lowerTarget = map toLower target
    lowerCandidate = map toLower candidate
    sortedTarget = sort lowerTarget
    sortedCandidate = sort lowerCandidate
