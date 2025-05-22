module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- Transform a single character using Atbash cipher
atbashChar :: Char -> Char
atbashChar c
  | isAlpha c = let lowerC = toLower c
                    offset = fromEnum lowerC - fromEnum 'a'
                    newOffset = 25 - offset
                in toEnum (fromEnum 'a' + newOffset)
  | otherwise = c

-- Remove non-alphanumeric characters and convert to lowercase
cleanText :: String -> String
cleanText = map toLower . filter (\c -> isAlpha c || isDigit c)

-- Group string into chunks of 5 characters separated by spaces
groupByFive :: String -> String
groupByFive [] = []
groupByFive xs = take 5 xs ++ if length xs > 5 then " " ++ groupByFive (drop 5 xs) else ""

decode :: String -> String
decode cipherText = map atbashChar (filter (/= ' ') cipherText)

encode :: String -> String
encode plainText = groupByFive (map atbashChar (cleanText plainText))
