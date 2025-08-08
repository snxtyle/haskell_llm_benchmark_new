module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    normalize :: String -> String
    normalize = sort . map toLower

    targetNorm = normalize target
    targetLower = map toLower target

    isAnagram :: String -> Bool
    isAnagram w =
      let wLower = map toLower w
      in wLower /= targetLower && normalize w == targetNorm
