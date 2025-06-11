module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)

-- | Encode a plaintext string using the Atbash cipher.
--   The output is lower-case, digits are kept unchanged,
--   and the result is grouped in blocks of five characters.
encode :: String -> String
encode plainText =
  unwords . chunksOf 5 $ map transform filtered
  where
    -- keep only letters and digits, normalise letters to lower-case
    filtered = [toLower c | c <- plainText, isAlpha c || isDigit c]
    transform c
      | isAlpha c = atbash c
      | otherwise = c

-- | Decode a ciphertext string that was produced by the Atbash cipher.
--   Spaces are ignored, letters are converted back via the same mapping,
--   and digits are preserved.
decode :: String -> String
decode cipherText = map transform filtered
  where
    -- remove spaces, keep letters & digits, normalise to lower-case
    filtered = [toLower c | c <- cipherText, isAlpha c || isDigit c]
    transform c
      | isAlpha c = atbash c
      | otherwise = c

-- | The Atbash substitution for a lower-case Latin letter.
atbash :: Char -> Char
atbash c = chr (ord 'a' + ord 'z' - ord c)

-- | Split a list into chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
