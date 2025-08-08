module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram xs =
  let letters = map toLower (filter isAlpha xs)
  in length letters == length (nub letters)
