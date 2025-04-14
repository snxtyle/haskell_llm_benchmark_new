module Atbash (decode, encode) where

import Data.Char (isLetter, isDigit, toLower, isLower)

-- Helper function to substitute a letter using Atbash cipher
substitute :: Char -> Char
substitute c
  | isLower c = toEnum (fromEnum 'a' + (25 - (fromEnum c - fromEnum 'a')))
  | otherwise = c  -- This case isn't expected but handles non-lowercase safely

-- Helper function to split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encode :: String -> String
encode plainText = unwords $ chunksOf 5 processed
  where
    lowerText = map toLower plainText  -- Convert to lowercase
    processed = [if isLetter c then substitute c else c | c <- lowerText, isLetter c || isDigit c]  -- Substitute letters, keep digits, filter others

decode :: String -> String
decode cipherText = [if isLetter c then substitute c else c | c <- map toLower cipherText, isLetter c || isDigit c]  -- Substitute letters, keep digits, filter others
