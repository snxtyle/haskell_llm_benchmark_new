module Atbash (decode, encode) where

import Data.Char (ord, chr, isLower, isDigit, toLower, isAlphaNum, isSpace)

-- | Translates a single character using the Atbash cipher.
-- Lowercase letters are transposed (a->z, b->y, etc.).
-- Digits are passed through unchanged.
atbashChar :: Char -> Char
atbashChar c
    | isLower c = chr $ ord 'z' - (ord c - ord 'a')
    | isDigit c = c
    | otherwise = c -- This case is for safety, though it should not be reached with the current logic.

-- | Groups a string into chunks of a given size.
group :: Int -> String -> [String]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

-- | Encodes a string using the Atbash cipher.
-- The output is grouped into 5-letter chunks, with numbers unchanged.
-- Punctuation is ignored.
encode :: String -> String
encode plainText =
    let cleaned = map toLower plainText
        filtered = filter isAlphaNum cleaned
        translated = map atbashChar filtered
    in unwords $ group 5 translated

-- | Decodes a string using the Atbash cipher.
-- The input is assumed to be valid Atbash ciphertext.
-- The output is a continuous string without spaces.
decode :: String -> String
decode cipherText =
    let filtered = filter (not . isSpace) cipherText
    in map atbashChar filtered
