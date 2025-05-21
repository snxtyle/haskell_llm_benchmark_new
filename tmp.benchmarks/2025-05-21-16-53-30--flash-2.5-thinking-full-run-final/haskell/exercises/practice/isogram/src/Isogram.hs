module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram s =
    let
        -- Convert the string to lowercase and filter out non-alphabetic characters.
        -- This handles case-insensitivity and ignores spaces/hyphens.
        processedChars = [toLower c | c <- s, isAlpha c]
        -- Get the list of unique characters from the processed string.
        uniqueChars = nub processedChars
    in
        -- An isogram has the same number of processed alphabetic characters
        -- as it has unique alphabetic characters.
        length processedChars == length uniqueChars
