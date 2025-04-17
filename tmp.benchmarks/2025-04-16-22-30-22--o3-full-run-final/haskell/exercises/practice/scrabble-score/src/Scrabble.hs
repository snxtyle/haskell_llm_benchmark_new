module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

-- | Compute the score of a single Scrabble letter.
--
--   The scoring is case–insensitive and falls back to 0 for any
--   character that is not a recognised Scrabble letter.
scoreLetter :: Char -> Integer
scoreLetter letter
  | up `elem` "AEIOULNRST" = 1
  | up `elem` "DG"         = 2
  | up `elem` "BCMP"       = 3
  | up `elem` "FHVWY"      = 4
  | up == 'K'              = 5
  | up `elem` "JX"         = 8
  | up `elem` "QZ"         = 10
  | otherwise              = 0
  where
    up = toUpper letter

-- | Compute the total Scrabble score for a word.
--
--   The function is case–insensitive and treats any non‑letter
--   characters as scoring 0 points.
scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
