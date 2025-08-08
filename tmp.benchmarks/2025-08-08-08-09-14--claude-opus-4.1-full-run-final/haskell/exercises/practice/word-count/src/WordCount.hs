module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount text = countOccurrences $ extractWords text

-- Extract words from text, handling punctuation and whitespace
extractWords :: String -> [String]
extractWords [] = []
extractWords text = 
    case nextWord text of
        ("", rest) -> extractWords (dropNonWord rest)
        (word, rest) -> word : extractWords rest

-- Get the next word and remaining text
nextWord :: String -> (String, String)
nextWord text = 
    let word = takeWhile isWordChar text
        rest = dropWhile isWordChar text
        cleanedWord = stripQuotes $ toLowerCase word
    in (cleanedWord, rest)

-- Strip leading and trailing apostrophes (quotes) but keep internal ones (contractions)
stripQuotes :: String -> String
stripQuotes s = dropWhile (== '\'') $ reverse $ dropWhile (== '\'') $ reverse s

-- Check if a character is part of a word (alphanumeric or apostrophe)
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\''

-- Drop characters that are not part of words
dropNonWord :: String -> String
dropNonWord [] = []
dropNonWord (x:xs) 
    | isWordChar x = x:xs
    | otherwise = xs

-- Convert string to lowercase
toLowerCase :: String -> String
toLowerCase = map toLower

-- Count occurrences of each word
countOccurrences :: [String] -> [(String, Int)]
countOccurrences wordList = 
    map countGroup $ group $ sort $ filter (not . null) wordList
  where
    countGroup [] = error "group should never be empty"
    countGroup ws@(w:_) = (w, length ws)
