module Isogram (isIsogram) where

import Data.List (nub)
import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram str = let filtered = filter (\c -> c /= ' ' && c /= '-') str
                    lowered = map toLower filtered
                    unique = nub lowered
                in length unique == length lowered
