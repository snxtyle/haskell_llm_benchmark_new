module Atbash (decode, encode) where

import Data.Char (toLower, isAlphaNum, isLetter)

decode :: String -> String
decode cipherText = map atbashChar $ filter isAlphaNum cipherText

encode :: String -> String
encode plainText = unwords $ chunksOf 5 $ map atbashChar $ filter isAlphaNum $ map toLower plainText

atbashChar :: Char -> Char
atbashChar c
  | isLetter c = chr $ ord 'z' - (ord (toLower c) - ord 'a')
  | otherwise = c

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum
