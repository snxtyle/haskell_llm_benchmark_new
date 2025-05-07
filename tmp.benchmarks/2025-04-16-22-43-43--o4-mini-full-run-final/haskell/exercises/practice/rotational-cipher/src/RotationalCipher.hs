module RotationalCipher (rotate) where

import Data.Char (ord, chr)

rotate :: Int -> String -> String
rotate n = map (rotateChar (n `mod` 26))

rotateChar :: Int -> Char -> Char
rotateChar n c
  | 'a' <= c && c <= 'z' = chr (((ord c - ord 'a' + n) `mod` 26) + ord 'a')
  | 'A' <= c && c <= 'Z' = chr (((ord c - ord 'A' + n) `mod` 26) + ord 'A')
  | otherwise            = c
