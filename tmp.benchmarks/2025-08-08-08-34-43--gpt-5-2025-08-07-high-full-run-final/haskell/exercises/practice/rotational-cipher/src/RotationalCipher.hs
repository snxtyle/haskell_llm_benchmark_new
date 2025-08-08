module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate n = map (shiftChar m)
  where
    m :: Int
    m = n `mod` 26

    shiftChar :: Int -> Char -> Char
    shiftChar s c
      | isLower c = rotateFrom 'a' s c
      | isUpper c = rotateFrom 'A' s c
      | otherwise = c

    rotateFrom :: Char -> Int -> Char -> Char
    rotateFrom base s c =
      chr $ ord base + ((ord c - ord base + s) `mod` 26)
