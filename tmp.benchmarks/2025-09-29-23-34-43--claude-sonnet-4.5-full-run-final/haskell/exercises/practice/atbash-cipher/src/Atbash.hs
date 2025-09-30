module Atbash (decode, encode) where

import Data.Char (isAlphaNum, isAlpha, toLower, ord, chr)
import Data.List (intercalate)

-- | Decode an Atbash cipher text
decode :: String -> String
decode cipherText = map atbashChar $ filter isAlphaNum $ map toLower cipherText

-- | Encode plain text using Atbash cipher
encode :: String -> String
encode plainText = groupByFive $ map atbashChar $ filter isAlphaNum $ map toLower plainText

-- | Apply Atbash transformation to a single character
-- Letters are transformed, digits are kept as-is
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = let offset = ord c - ord 'a'
                    reversed = 25 - offset
                in chr (ord 'a' + reversed)
  | otherwise = c

-- | Group a string into chunks of 5 characters separated by spaces
groupByFive :: String -> String
groupByFive [] = []
groupByFive s = intercalate " " $ chunksOf 5 s

-- | Split a list into chunks of size n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
