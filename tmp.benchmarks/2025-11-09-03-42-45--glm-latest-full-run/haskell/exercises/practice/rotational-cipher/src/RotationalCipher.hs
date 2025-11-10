module RotationalCipher (rotate) where

import Data.Char (ord, chr, isLower, isUpper)

-- | Rotates a string by a given key using a rotational cipher.
--   Only alphabetic characters are rotated. Case is preserved.
rotate :: Int -> String -> String
rotate key text = map (rotateChar effectiveKey) text
  where
    -- The effective key is the key modulo 26, since there are 26 letters.
    effectiveKey = key `mod` 26

    -- Helper function to rotate a single character.
    rotateChar :: Int -> Char -> Char
    rotateChar k c
      | isLower c = shift 'a' c k
      | isUpper c = shift 'A' c k
      | otherwise = c

    -- Helper function to perform the actual shift.
    -- It takes the base character ('a' or 'A'), the character to shift,
    -- and the key.
    shift :: Char -> Char -> Int -> Char
    shift base char key =
      let originalPos = ord char - ord base
          newPos     = (originalPos + key) `mod` 26
          newChar    = chr (ord base + newPos)
      in newChar
