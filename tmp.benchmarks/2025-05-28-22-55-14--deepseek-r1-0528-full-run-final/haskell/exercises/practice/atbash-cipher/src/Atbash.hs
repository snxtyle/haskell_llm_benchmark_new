module Atbash (decode, encode) where

import Data.Char (isAlphaNum, toLower, ord, chr)

decode :: String -> String
decode cipherText = map atbashChar $ filter isAlphaNum $ map toLower cipherText

encode :: String -> String
encode plainText = unwords $ chunksOf 5 transformed
  where
    cleaned = filter isAlphaNum $ map toLower plainText
    transformed = map atbashChar cleaned
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

atbashChar :: Char -> Char
atbashChar c
  | 'a' <= c && c <= 'z' = chr (ord 'z' - (ord c - ord 'a'))
  | otherwise = c
