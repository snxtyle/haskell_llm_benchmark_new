module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate)

-- | Convert a single alphabetic character using the Atbash cipher.
--   Non‑alphabetic characters are returned unchanged.
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = chr (ord 'a' + ord 'z' - ord (toLower c))
  | otherwise = c

-- | Keep only characters we are supposed to encode/decode
--   (letters and digits). Everything else is discarded.
sanitize :: String -> String
sanitize = filter (\x -> isAlpha x || isDigit x)

-- | Break a list into chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Encode a plaintext string using the Atbash cipher.
--   The output is grouped in blocks of five characters,
--   separated by spaces.
encode :: String -> String
encode plainText =
  intercalate " " . chunksOf 5 $ map atbashChar (sanitize plainText)

-- | Decode an Atbash‑encoded string back to plaintext.
--   Grouping spaces and punctuation are ignored.
decode :: String -> String
decode cipherText = map atbashChar (sanitize cipherText)
