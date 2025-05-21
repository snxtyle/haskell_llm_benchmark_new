module RotationalCipher (rotate) where

import Data.Char (isAsciiLower, isAsciiUpper, ord, chr)

-- | Rotates a single character based on the given key.
-- Only rotates ASCII letters; other characters are returned as is.
rotateChar :: Int -> Char -> Char
rotateChar k c
  | isAsciiLower c = rotateLetter 'a' c k
  | isAsciiUpper c = rotateLetter 'A' c k
  | otherwise      = c

-- | Helper function to rotate an ASCII letter.
-- 'baseChar' should be 'a' for lowercase or 'A' for uppercase.
rotateLetter :: Char -> Char -> Int -> Char
rotateLetter baseChar char key =
  let
    base = ord baseChar
    -- Calculate the 0-indexed position of the character within its alphabet (e.g., 'a' is 0, 'b' is 1)
    charOffset = ord char - base
    -- Ensure the key is positive and within the 0-25 range
    effectiveKey = (key `mod` 26 + 26) `mod` 26
    -- Calculate the new 0-indexed position after shifting
    shiftedOffset = (charOffset + effectiveKey) `mod` 26
  in
    -- Convert the new 0-indexed position back to a character
    chr (base + shiftedOffset)

-- | Encodes a string using a rotational cipher with the given key.
-- The key determines the number of positions each letter is shifted.
-- Non-alphabetic characters are not shifted.
rotate :: Int -> String -> String
rotate k s = map (rotateChar k) s
