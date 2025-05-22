module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isDigit, toLower, ord, chr)
import Data.List (intercalate)

-- Helper function to apply Atbash transformation to a single character
atbashChar :: Char -> Char
atbashChar c
  | isDigit c = c
  | otherwise = chr (ord 'z' - (ord c - ord 'a'))

-- Helper function to group a string into chunks of n characters
chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

decode :: String -> String
decode cipherText = map atbashChar $ filter (/= ' ') cipherText

encode :: String -> String
encode plainText = intercalate " " $ chunksOf 5 $ map atbashChar normalized
  where
    normalized = filter isAlphaNum $ map toLower plainText
