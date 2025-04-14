module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = all (`elem` uniqueLowerLetters) ['a'..'z']
  where
    lowerText = map toLower text  -- Convert to lowercase
    letters = filter isAlpha lowerText  -- Keep only alphabetic characters
    uniqueLowerLetters = nub letters  -- Get unique letters
