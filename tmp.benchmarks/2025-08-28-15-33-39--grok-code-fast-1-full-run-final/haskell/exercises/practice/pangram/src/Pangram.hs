module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = length (nub (map toLower (filter (\c -> c >= 'a' && c <= 'z') text))) == 26
