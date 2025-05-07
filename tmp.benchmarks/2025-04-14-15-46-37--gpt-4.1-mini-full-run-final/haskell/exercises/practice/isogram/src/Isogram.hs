module Isogram (isIsogram) where

import Data.Char (toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str =
  let filtered = [toLower c | c <- str, c /= ' ', c /= '-']
  in length filtered == length (nub filtered)
