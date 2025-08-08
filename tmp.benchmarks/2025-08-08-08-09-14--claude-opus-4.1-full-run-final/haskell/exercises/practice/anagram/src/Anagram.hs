module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter (isAnagramOf target) candidates

isAnagramOf :: String -> String -> Bool
isAnagramOf target candidate =
  let targetLower = map toLower target
      candidateLower = map toLower candidate
  in targetLower /= candidateLower && sort targetLower == sort candidateLower
