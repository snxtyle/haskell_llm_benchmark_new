module Pangram (isPangram) where

import Data.Char (toLower) -- isAlpha is no longer needed for filtering
import Data.Set (fromList, size)

isPangram :: String -> Bool
isPangram text =
    let
        -- Convert text to lowercase and filter for characters that are
        -- specifically English alphabet letters (a-z).
        englishAlphaChars = filter (\c -> c >= 'a' && c <= 'z') (map toLower text)
        -- Create a set of unique English alphabet characters found
        uniqueLetters = fromList englishAlphaChars
        -- Check if the size of the set is 26 (for all English alphabet letters)
    in size uniqueLetters == 26
