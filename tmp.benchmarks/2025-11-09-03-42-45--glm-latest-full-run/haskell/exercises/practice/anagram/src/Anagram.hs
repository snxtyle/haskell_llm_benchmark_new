module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter (isAnagram target) candidates
  where
    isAnagram :: String -> String -> Bool
    isAnagram t c
      | map toLower t == map toLower c = False
      | otherwise = sort (map toLower t) == sort (map toLower c)
