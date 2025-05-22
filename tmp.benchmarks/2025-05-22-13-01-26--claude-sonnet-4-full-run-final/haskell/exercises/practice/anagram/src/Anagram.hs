module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor target candidates = filter isAnagram candidates
  where
    normalizedTarget = sort (map toLower target)
    isAnagram candidate = 
      map toLower candidate /= map toLower target &&  -- Not the same word (case insensitive)
      sort (map toLower candidate) == normalizedTarget -- Same letters when sorted
