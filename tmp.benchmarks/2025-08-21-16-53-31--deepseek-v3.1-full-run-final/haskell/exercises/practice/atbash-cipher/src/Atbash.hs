module Atbash (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower)

-- Create the Atbash mapping
atbashMap :: Char -> Char
atbashMap c
    | c >= 'a' && c <= 'z' = toEnum (fromEnum 'z' - fromEnum c + fromEnum 'a')
    | otherwise = c

-- Helper function to process text for encoding/decoding
processText :: String -> (Char -> Char) -> String
processText text transform = map transform $ filter (\c -> isAlpha c || isDigit c) (map toLower text)

-- Decode cipher text (remove spaces, apply mapping)
decode :: String -> String
decode cipherText = processText (filter (/= ' ') cipherText) atbashMap

-- Encode plain text (apply mapping and group into 5-letter chunks)
encode :: String -> String
encode plainText = unwords $ splitIntoGroups 5 (processText plainText atbashMap)
  where
    splitIntoGroups _ [] = []
    splitIntoGroups n str = 
        let (group, rest) = splitAt n str
        in group : splitIntoGroups n rest
