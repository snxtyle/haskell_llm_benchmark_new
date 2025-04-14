module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)

isPangram :: String -> Bool
isPangram text = all (`elem` letters) ['a'..'z']
  where
    letters = map toLower $ filter isAlpha text
