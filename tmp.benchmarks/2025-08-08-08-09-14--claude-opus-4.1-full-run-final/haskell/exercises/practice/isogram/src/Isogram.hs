module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = length letters == length (nub letters)
  where
    letters = map toLower $ filter isLetter str
