module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram xs =
  let letters = map toLower (filter isLetter xs)
   in length letters == length (nub letters)
