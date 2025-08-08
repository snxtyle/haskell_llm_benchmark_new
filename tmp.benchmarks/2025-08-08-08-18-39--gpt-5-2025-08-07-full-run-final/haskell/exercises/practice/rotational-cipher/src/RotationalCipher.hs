module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, ord, chr)

rotate :: Int -> String -> String
rotate n = map (shift k)
  where
    k = n `mod` 26
    shift :: Int -> Char -> Char
    shift m c
      | isLower c = chr $ ord 'a' + ((ord c - ord 'a' + m) `mod` 26)
      | isUpper c = chr $ ord 'A' + ((ord c - ord 'A' + m) `mod` 26)
      | otherwise = c
