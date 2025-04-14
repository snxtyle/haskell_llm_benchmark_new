module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter = case toUpper letter of
    'A' -> 1  -- A, E, I, O, U, L, N, R, S, T
    'E' -> 1
    'I' -> 1
    'O' -> 1
    'U' -> 1
    'L' -> 1
    'N' -> 1
    'R' -> 1
    'S' -> 1
    'T' -> 1
    'D' -> 2  -- D, G
    'G' -> 2
    'B' -> 3  -- B, C, M, P
    'C' -> 3
    'M' -> 3
    'P' -> 3
    'F' -> 4  -- F, H, V, W, Y
    'H' -> 4
    'V' -> 4
    'W' -> 4
    'Y' -> 4
    'K' -> 5  -- K
    'J' -> 8  -- J, X
    'X' -> 8
    'Q' -> 10 -- Q, Z
    'Z' -> 10
    _   -> 0  -- Non-letter characters get a score of 0

scoreWord :: String -> Integer
scoreWord word = sum [scoreLetter c | c <- word]  -- Sum the scores of all letters in the word
