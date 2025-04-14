module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = 
    let lowered = map toLower str  -- Convert to lowercase
        onlyLetters = filter isLetter lowered  -- Keep only letters
    in length onlyLetters == length (nub onlyLetters)  -- Check for uniqueness
