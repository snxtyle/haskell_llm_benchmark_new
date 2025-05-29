module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram s = length letters == length (nub letters)
  where
    letters = [ toLower c | c <- s, isLetter c ]
