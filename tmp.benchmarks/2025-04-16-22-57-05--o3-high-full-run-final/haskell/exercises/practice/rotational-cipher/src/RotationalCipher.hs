module RotationalCipher (rotate) where

import Data.Char (isLower, isUpper, chr, ord)

-- | Rotate a string by the given key (0‑26).
--   Letters are shifted, preserving case.
--   All non‑alphabetic characters are kept as‑is.
rotate :: Int -> String -> String
rotate key = map (rotateChar shift)
  where
    shift = key `mod` 26          -- ensure we’re inside 0..25

-- | Shift a single character, if it is alphabetic.
rotateChar :: Int -> Char -> Char
rotateChar s c
  | isLower c = chr $ base 'a' + offset 'a'
  | isUpper c = chr $ base 'A' + offset 'A'
  | otherwise = c
  where
    base ch   = ord ch
    offset ch = (ord c - base ch + s) `mod` 26
