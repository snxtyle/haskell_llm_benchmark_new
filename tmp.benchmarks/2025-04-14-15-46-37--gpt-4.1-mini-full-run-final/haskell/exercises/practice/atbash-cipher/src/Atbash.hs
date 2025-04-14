module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, chr, ord)

-- Map a lowercase letter to its Atbash counterpart
atbashChar :: Char -> Char
atbashChar c
  | 'a' <= c && c <= 'z' = chr (ord 'z' - (ord c - ord 'a'))
  | otherwise = c

-- Encode a string using Atbash cipher
encode :: String -> String
encode plainText = unwords $ group5 $ map encodeChar filtered
  where
    -- Filter to only letters and digits, convert letters to lowercase
    filtered = [toLower c | c <- plainText, isAlpha c || isDigit c]
    -- Encode letters with atbash, digits unchanged
    encodeChar c
      | isAlpha c = atbashChar c
      | otherwise = c
    -- Group into chunks of 5 characters
    group5 [] = []
    group5 xs = take 5 xs : group5 (drop 5 xs)

-- Decode a string using Atbash cipher
decode :: String -> String
decode cipherText = map decodeChar filtered
  where
    -- Remove spaces
    filtered = [c | c <- cipherText, c /= ' ']
    -- Decode letters with atbash, digits unchanged
    decodeChar c
      | isAlpha c = atbashChar (toLower c)
      | otherwise = c
