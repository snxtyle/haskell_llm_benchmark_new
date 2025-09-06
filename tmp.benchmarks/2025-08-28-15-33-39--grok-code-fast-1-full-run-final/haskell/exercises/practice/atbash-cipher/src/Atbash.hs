module Atbash (decode, encode) where

import Data.Char (toLower, isAlphaNum, isLower, chr, ord)

encodeChar :: Char -> Char
encodeChar c
  | isLower c = chr (ord 'a' + ord 'z' - ord c)
  | otherwise = c

group5 :: String -> [String]
group5 [] = []
group5 xs = take 5 xs : group5 (drop 5 xs)

encode :: String -> String
encode plainText = unwords $ group5 $ map encodeChar $ filter isAlphaNum $ map toLower plainText

decode :: String -> String
decode cipherText = map encodeChar $ filter (/= ' ') cipherText
