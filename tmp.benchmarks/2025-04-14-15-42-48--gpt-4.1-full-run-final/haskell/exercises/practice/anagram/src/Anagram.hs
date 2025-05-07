module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter isAnagram xss
  where
    norm = map toLower
    sorted = sort . norm
    target = norm xs
    isAnagram candidate =
      let candidateNorm = norm candidate
      in candidateNorm /= target && sorted candidate == sorted xs
