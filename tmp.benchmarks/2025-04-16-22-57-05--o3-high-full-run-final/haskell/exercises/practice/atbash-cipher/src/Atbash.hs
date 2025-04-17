module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)

-- | Translate a single alphabetic character using the Atbash cipher.
--   Assumes the input is a *lower‑case* letter.
atbash :: Char -> Char
atbash c = chr (ord 'a' + (25 - (ord c - ord 'a')))

-- | Break a list into sub‑lists of length @n@.
groupOf :: Int -> [a] -> [[a]]
groupOf _ [] = []
groupOf n xs = take n xs : groupOf n (drop n xs)

-- | Encode a plaintext string with the Atbash cipher.
--   Output is lower‑case, grouped in blocks of five characters.
encode :: String -> String
encode plainText = unwords (groupOf 5 encoded)
  where
    encoded = [ encodeChar c | c <- plainText, keep c ]

    keep c = isAlpha c || isDigit c

    encodeChar c
      | isAlpha c = atbash (toLower c)
      | otherwise = c  -- digits are left unchanged

-- | Decode a ciphertext string that was produced by the Atbash cipher.
--   Spaces and punctuation are ignored; output is a continuous lower‑case string.
decode :: String -> String
decode cipherText = map decodeChar $ filter keep cipherText
  where
    keep c = isAlpha c || isDigit c

    decodeChar c
      | isAlpha c = atbash (toLower c)
      | otherwise = c  -- digits are left unchanged
