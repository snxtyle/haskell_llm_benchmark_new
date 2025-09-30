module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = countWords $ extractWords xs

-- Extract words from the input string
extractWords :: String -> [String]
extractWords = map (map toLower) . filter (not . null) . map stripApostrophes . filter (not . null) . splitWords

-- Split the string into words, preserving apostrophes within words
splitWords :: String -> [String]
splitWords [] = []
splitWords str = 
    let (word, rest) = extractWord str
    in if null word
       then splitWords rest
       else word : splitWords rest

-- Extract a single word from the beginning of the string
extractWord :: String -> (String, String)
extractWord [] = ("", "")
extractWord (c:cs)
    | isWordChar c = 
        let (word, rest) = extractWord cs
        in (c:word, rest)
    | otherwise = ("", cs)

-- Check if a character is part of a word (alphanumeric or apostrophe)
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\''

-- Strip leading and trailing apostrophes from a word
stripApostrophes :: String -> String
stripApostrophes = dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse

-- Count occurrences of each word
countWords :: [String] -> [(String, Int)]
countWords ws = map (\group' -> (head group', length group')) $ group $ sort ws
