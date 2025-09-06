module Isogram (isIsogram) where

import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram str = all noDuplicate letters
  where
    -- Filter only letters and convert to lowercase
    letters = map toLower $ filter isLetter str
    -- Check if a character appears only once in the list
    noDuplicate c = length (filter (== c) letters) == 1
    -- Helper function to check if character is a letter
    isLetter c = c /= ' ' && c /= '-'
