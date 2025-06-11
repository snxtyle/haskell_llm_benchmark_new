module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)
import Data.List ((\\))

-- | Determine whether a given sentence is a pangram.
--   A pangram contains every letter of the English alphabet
--   at least once, case-insensitively.
isPangram :: String -> Bool
isPangram sentence = null missing
  where
    letters = [toLower c | c <- sentence, isAlpha c]
    missing = ['a' .. 'z'] \\ letters
