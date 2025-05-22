module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = map (\ws -> (head ws, length ws)) $ group $ sort $ words' xs
  where
    words' :: String -> [String]
    words' = filter (not . null) . map (map toLower) . splitWords
    
    splitWords :: String -> [String]
    splitWords [] = []
    splitWords str = 
      let (word, rest) = span isWordChar str
          cleanWord = stripQuotes word
      in if null cleanWord
         then splitWords (dropWhile (not . isWordChar) rest)
         else cleanWord : splitWords (dropWhile (not . isWordChar) rest)
    
    isWordChar :: Char -> Bool
    isWordChar c = isAlphaNum c || c == '\''
    
    stripQuotes :: String -> String
    stripQuotes word = 
      let stripped = dropWhile (== '\'') $ reverse $ dropWhile (== '\'') $ reverse word
      in if null stripped then word else stripped
