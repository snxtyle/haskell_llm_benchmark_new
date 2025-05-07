module RotationalCipher (rotate) where

import Data.Char (isAsciiLower, isAsciiUpper, ord, chr)

rotate :: Int -> String -> String
rotate key text = map (rotateChar effectiveKey) text
  where
    effectiveKey = key `mod` 26

rotateChar :: Int -> Char -> Char
rotateChar key char
  | isAsciiLower char = shiftChar 'a' key char
  | isAsciiUpper char = shiftChar 'A' key char
  | otherwise         = char

shiftChar :: Char -> Int -> Char -> Char
shiftChar base key char = chr (ord base + (ord char - ord base + key) `mod` 26)
