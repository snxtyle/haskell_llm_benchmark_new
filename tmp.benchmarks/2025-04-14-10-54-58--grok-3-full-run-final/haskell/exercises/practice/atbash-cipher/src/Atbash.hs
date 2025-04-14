module Atbash (decode, encode) where

import Data.Char (isLetter, isDigit, toLower)
import Data.List (intersperse)

-- Helper function to transform a single character using Atbash cipher
transformChar :: Char -> Char
transformChar c
  | isLetter c = let base = fromEnum 'a' in
                 toEnum (base + (25 - (fromEnum (toLower c) - base)))
  | otherwise = c

-- Decode a cipher text back to plain text
decode :: String -> String
decode cipherText = map transformChar cipherText

-- Encode plain text to cipher text with grouping of 5 characters
encode :: String -> String
encode plainText = unwords $ chunksOf 5 encoded
  where
    -- Filter out non-alphanumeric characters and transform letters
    encoded = map transformChar $ filter (\c -> isLetter c || isDigit c) $ map toLower plainText
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
