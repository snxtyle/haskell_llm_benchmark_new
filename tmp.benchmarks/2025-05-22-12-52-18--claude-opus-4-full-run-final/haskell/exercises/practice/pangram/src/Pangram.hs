module Pangram (isPangram) where

import Data.Char (toLower, isLetter)

isPangram :: String -> Bool
isPangram text = all (`elem` lowerLetters) ['a'..'z']
  where
    lowerLetters = map toLower $ filter isLetter text
