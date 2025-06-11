module Isogram (isIsogram) where

import Data.Char (toLower, isLetter)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram phrase =
    let letters = [toLower c | c <- phrase, isLetter c]
    in length letters == length (nub letters)
