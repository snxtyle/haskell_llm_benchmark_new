module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)

-- | Determine whether a text is a pangram.
--   A pangram contains every letter of the English alphabet
--   at least once, case–insensitively.
isPangram :: String -> Bool
isPangram text =
  let letters = [toLower c | c <- text, isAlpha c]
  in all (`elem` letters) ['a' .. 'z']
