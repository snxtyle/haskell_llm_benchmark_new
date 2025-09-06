module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate shift = map (shiftChar (shift `mod` 26))

shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isLower c = chr (ord 'a' + (ord c - ord 'a' + shift) `mod` 26)
  | isUpper c = chr (ord 'A' + (ord c - ord 'A' + shift) `mod` 26)
  | otherwise = c
