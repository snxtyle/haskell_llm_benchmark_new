module Pangram (isPangram) where

import Data.Char (toLower, isAsciiLower)

isPangram :: String -> Bool
isPangram text =
  let letters = filter isAsciiLower $ map toLower text
      uniqueLetters = removeDuplicates letters
  in length uniqueLetters == 26

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
