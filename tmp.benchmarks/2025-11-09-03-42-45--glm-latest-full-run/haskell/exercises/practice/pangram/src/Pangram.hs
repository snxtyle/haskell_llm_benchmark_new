module Pangram (isPangram) where

import Data.Set (Set, fromList, size)
import Data.Char (toLower, isAscii, isLower)

-- | Checks if a string is a pangram.
-- A pangram is a sentence using every letter of the alphabet at least once.
isPangram :: String -> Bool
isPangram text = size uniqueLetters == 26
  where
    lowerCaseText = map toLower text
    lettersOnly = filter (\c -> isAscii c && isLower c) lowerCaseText
    uniqueLetters :: Set Char
    uniqueLetters = fromList lettersOnly
