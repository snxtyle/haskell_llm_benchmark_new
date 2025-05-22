module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate key = map (rotateChar normalizedKey)
  where
    normalizedKey = key `mod` 26

rotateChar :: Int -> Char -> Char
rotateChar key c
  | isLower c = shiftChar 'a' key c
  | isUpper c = shiftChar 'A' key c
  | otherwise = c

shiftChar :: Char -> Int -> Char -> Char
shiftChar base key c = chr $ ord base + ((ord c - ord base + key) `mod` 26)
