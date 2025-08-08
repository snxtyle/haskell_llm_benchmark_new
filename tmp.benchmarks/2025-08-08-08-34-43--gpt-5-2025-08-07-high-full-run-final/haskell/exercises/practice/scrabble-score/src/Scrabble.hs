module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter c
  | u `elem` "AEIOULNRST" = 1
  | u `elem` "DG"         = 2
  | u `elem` "BCMP"       = 3
  | u `elem` "FHVWY"      = 4
  | u == 'K'              = 5
  | u `elem` "JX"         = 8
  | u `elem` "QZ"         = 10
  | otherwise             = 0
  where
    u = toUpper c

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
