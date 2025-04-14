module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    target = map toLower xs
    sortedTarget = sort target
    isAnagram candidate =
      let candidateLower = map toLower candidate
      in candidateLower /= target && sort candidateLower == sortedTarget
