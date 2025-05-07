module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text =
  let lowerText = map toLower text
      alphabet = ['a'..'z']
  in all (\c -> c `elem` lowerText) alphabet
