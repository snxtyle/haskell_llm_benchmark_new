module Isogram (isIsogram) where

import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram str = length filteredChars == length uniqueChars
  where
    normalizedStr = map toLower str
    filteredChars = filter (\c -> c /= ' ' && c /= '-') normalizedStr
    uniqueChars = removeDuplicates filteredChars

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
