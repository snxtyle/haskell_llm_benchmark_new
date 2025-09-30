module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = all (`elem` letters) ['a'..'z']
  where
    letters = nub $ map toLower $ filter isAlpha text
