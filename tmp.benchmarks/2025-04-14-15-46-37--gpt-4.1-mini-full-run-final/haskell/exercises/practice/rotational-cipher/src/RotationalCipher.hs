module RotationalCipher (rotate) where

import Data.Char (chr, ord, isUpper, isLower)

rotate :: Int -> String -> String
rotate n = map (rotateChar n)

rotateChar :: Int -> Char -> Char
rotateChar n c
  | isUpper c = chr $ baseUpper + ((ord c - baseUpper + n) `mod` 26)
  | isLower c = chr $ baseLower + ((ord c - baseLower + n) `mod` 26)
  | otherwise = c
  where
    baseUpper = ord 'A'
    baseLower = ord 'a'
