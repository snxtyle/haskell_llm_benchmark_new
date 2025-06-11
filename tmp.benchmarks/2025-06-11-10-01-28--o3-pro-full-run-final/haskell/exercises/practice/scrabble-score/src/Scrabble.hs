module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

-- | Compute the score for a single Scrabble tile.
--   Non-alphabetic characters are treated as having a score of 0.
scoreLetter :: Char -> Integer
scoreLetter letter = case toUpper letter of
  'A' -> 1
  'E' -> 1
  'I' -> 1
  'O' -> 1
  'U' -> 1
  'L' -> 1
  'N' -> 1
  'R' -> 1
  'S' -> 1
  'T' -> 1
  'D' -> 2
  'G' -> 2
  'B' -> 3
  'C' -> 3
  'M' -> 3
  'P' -> 3
  'F' -> 4
  'H' -> 4
  'V' -> 4
  'W' -> 4
  'Y' -> 4
  'K' -> 5
  'J' -> 8
  'X' -> 8
  'Q' -> 10
  'Z' -> 10
  _   -> 0

-- | Compute the total Scrabble score of a word.
--   The function is case-insensitive and ignores any non-alphabetic
--   characters (they contribute 0 points).
scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
