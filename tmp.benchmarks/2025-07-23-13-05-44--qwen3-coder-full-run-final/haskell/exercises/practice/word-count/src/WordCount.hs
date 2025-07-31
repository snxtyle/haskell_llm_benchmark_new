module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount = map (\ws -> (head ws, length ws)) . group . sort . filter (not . null) . wordsOnly . map toLower
  where
    wordsOnly :: String -> [String]
    wordsOnly [] = []
    wordsOnly (c:cs)
      | isWordChar c = 
          let (word, rest) = span isWordChar (c:cs)
              cleanedWord = removeLeadingTrailingApostrophes word
          in cleanedWord : wordsOnly rest
      | otherwise = wordsOnly cs
    
    isWordChar :: Char -> Bool
    isWordChar c = isAlphaNum c || c == '\''
    
    removeLeadingTrailingApostrophes :: String -> String
    removeLeadingTrailingApostrophes = reverse . dropWhile (== '\'') . reverse . dropWhile (== '\'')
