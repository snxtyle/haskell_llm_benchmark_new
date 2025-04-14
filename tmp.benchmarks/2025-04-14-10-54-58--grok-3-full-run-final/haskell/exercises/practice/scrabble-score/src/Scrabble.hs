module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

-- Mapping of letters to their Scrabble scores
scoreLetter :: Char -> Integer
scoreLetter letter = case toUpper letter of
    c | c `elem` "AEIOULNRST" -> 1
    c | c `elem` "DG"         -> 2
    c | c `elem` "BCMP"       -> 3
    c | c `elem` "FHVWY"      -> 4
    c | c `elem` "K"          -> 5
    c | c `elem` "JX"         -> 8
    c | c `elem` "QZ"         -> 10
    _                         -> 0

-- Calculate total score for a word by summing the scores of its letters
scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word
