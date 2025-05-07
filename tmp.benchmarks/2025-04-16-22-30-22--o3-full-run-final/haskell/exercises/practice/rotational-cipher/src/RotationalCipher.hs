module RotationalCipher (rotate) where

import Data.Char (chr, ord, isLower, isUpper)

-- | Apply a rotational (Caesar) cipher to a given string.
--   The key is interpreted modulo 26, so keys of 0 or 26
--   leave the input unchanged.
--
--   Only alphabetic characters are shifted; all other
--   characters (spaces, punctuation, digits, etc.) are
--   left intact.  Case is preserved.
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
rotate key = map shiftChar
  where
    k = key `mod` 26  -- normalise the key to the range [0,25]
    shiftBase base c = chr $ (ord c - base + k) `mod` 26 + base

    shiftChar c
      | isLower c = shiftBase (ord 'a') c
      | isUpper c = shiftBase (ord 'A') c
      | otherwise = c
