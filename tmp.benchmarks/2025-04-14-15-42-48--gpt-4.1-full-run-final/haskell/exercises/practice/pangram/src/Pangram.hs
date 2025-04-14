module Pangram (isPangram) where

import Data.Char (isAscii, isAlpha, toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text =
  let letters = [toLower c | c <- text, isAscii c, isAlpha c]
      uniqueLetters = nub letters
  in length uniqueLetters == 26
