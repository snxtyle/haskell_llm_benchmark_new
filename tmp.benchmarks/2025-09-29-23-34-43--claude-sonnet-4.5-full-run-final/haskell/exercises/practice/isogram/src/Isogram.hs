module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = letters == nub letters
  where
    letters = map toLower $ filter isLetter str
