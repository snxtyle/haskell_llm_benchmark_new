module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount text = countWords $ extractWords $ map toLower text
  where
    -- Extract words from the text, handling contractions properly
    extractWords :: String -> [String]
    extractWords [] = []
    extractWords str = 
      case span isWordChar str of
        ([], rest) -> extractWords (dropWhile (not . isWordChar) rest)
        (word, rest) -> stripQuotes word : extractWords rest
    
    -- Check if a character is part of a word (alphanumeric or apostrophe)
    isWordChar :: Char -> Bool
    isWordChar c = isAlphaNum c || c == '\''
    
    -- Strip leading and trailing apostrophes (quotes), but keep internal ones (contractions)
    stripQuotes :: String -> String
    stripQuotes = dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse
    
    -- Count occurrences of each word
    countWords :: [String] -> [(String, Int)]
    countWords wordList = map (\ws -> (head ws, length ws)) $ group $ sort $ filter (not . null) wordList
