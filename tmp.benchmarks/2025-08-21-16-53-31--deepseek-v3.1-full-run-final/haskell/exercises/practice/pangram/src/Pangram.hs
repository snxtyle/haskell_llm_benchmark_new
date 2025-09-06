module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)

isPangram :: String -> Bool
isPangram text = all (`elem` lowerText) ['a'..'z']
    where lowerText = map toLower (filter isAlpha text)
