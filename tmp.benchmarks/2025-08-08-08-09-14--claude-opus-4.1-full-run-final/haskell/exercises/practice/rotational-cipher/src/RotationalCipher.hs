module RotationalCipher (rotate) where

import Data.Char (isLetter, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar key)

rotateChar :: Int -> Char -> Char
rotateChar key c
  | not (isLetter c) = c
  | isUpper c = rotateInRange 'A' 'Z' key c
  | otherwise = rotateInRange 'a' 'z' key c

rotateInRange :: Char -> Char -> Int -> Char -> Char
rotateInRange start end key c = chr $ start' + ((ord c - start' + key) `mod` 26)
  where start' = ord start
