module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isDigit, toLower, ord, chr)
import Data.List (intercalate)

-- Helper function to transform a single character using Atbash
atbashChar :: Char -> Char
atbashChar c
  | isDigit c = c
  | c >= 'a' && c <= 'z' = chr (ord 'z' - (ord c - ord 'a'))
  | otherwise = c

-- Helper function to chunk a string into groups of n characters
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

decode :: String -> String
decode cipherText = map atbashChar $ filter (/= ' ') cipherText

encode :: String -> String
encode plainText = intercalate " " $ chunksOf 5 encoded
  where
    -- Convert to lowercase, keep only alphanumeric characters
    cleaned = filter isAlphaNum $ map toLower plainText
    -- Apply Atbash transformation
    encoded = map atbashChar cleaned
