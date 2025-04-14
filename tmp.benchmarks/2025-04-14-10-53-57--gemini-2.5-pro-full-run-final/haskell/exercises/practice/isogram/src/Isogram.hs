module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram text = allUnique processedChars
  where
    -- Convert to lowercase and keep only letters
    processedChars :: String
    processedChars = map toLower $ filter isLetter text

    -- Check if all characters in a list are unique
    allUnique :: Eq a => [a] -> Bool
    allUnique xs = length xs == length (nub xs)
