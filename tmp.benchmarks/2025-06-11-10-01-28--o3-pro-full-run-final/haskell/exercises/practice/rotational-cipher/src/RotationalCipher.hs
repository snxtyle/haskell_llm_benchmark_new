module RotationalCipher (rotate) where

import Data.Char (chr, ord, isAsciiLower, isAsciiUpper)

-- | Rotate a string using a Caesar cipher.
--   Non-alphabetic characters are left unchanged.
--
--   The rotation amount can be any integral value: it is taken modulo 26,
--   so values like 0 and 26 yield the identity transformation and negative
--   values rotate in the opposite direction.
--
--   Examples:
--
--   >>> rotate 13 "The quick brown fox jumps over the lazy dog."
--   "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
--
--   >>> rotate 13 "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
--   "The quick brown fox jumps over the lazy dog."
--
--   >>> rotate 5 "omg"
--   "trl"
rotate :: Int -> String -> String
rotate n = map (rotateChar k)
  where
    k = n `mod` 26  -- normalise rotation to the range [0,25]

-- Rotate a single character, preserving case.
rotateChar :: Int -> Char -> Char
rotateChar k c
  | isAsciiLower c = shift 'a'
  | isAsciiUpper c = shift 'A'
  | otherwise      = c
  where
    shift base = chr $ (ord c - ord base + k) `mod` 26 + ord base
