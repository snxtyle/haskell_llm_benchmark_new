module Atbash (decode, encode) where

import Data.Char (isAlpha, isAlphaNum, toLower, ord, chr)

-- Map a single character using the Atbash cipher.
-- Letters are mapped to their reversed alphabet counterpart (in lowercase),
-- digits are left unchanged. Non-alphanumerics are filtered out before use.
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = chr (ord 'a' + ord 'z' - ord (toLower c))
  | otherwise = c

-- Keep only alphanumeric characters and lowercase letters
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Split a list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: String -> String
encode = unwords . chunksOf 5 . map atbashChar . normalize

decode :: String -> String
decode = map atbashChar . normalize
