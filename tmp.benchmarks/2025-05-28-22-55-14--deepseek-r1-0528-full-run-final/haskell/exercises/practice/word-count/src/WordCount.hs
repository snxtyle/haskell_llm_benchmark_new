module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (sort, group)

wordCount :: String -> [(String, Int)]
wordCount text = 
    let lowerText = map toLower text
        tokens = splitKeepBy isWordChar lowerText
        cleanedTokens = map stripApostrophes tokens
        nonEmptyTokens = filter (not . null) cleanedTokens
        sorted = sort nonEmptyTokens
        grouped = group sorted
    in [ (head g, length g) | g <- grouped ]
  where
    isWordChar c = isAlphaNum c || c == '\''

    stripApostrophes :: String -> String
    stripApostrophes = 
        reverse . dropWhile (=='\'') . reverse . dropWhile (=='\'')

    splitKeepBy :: (Char -> Bool) -> String -> [String]
    splitKeepBy _ [] = []
    splitKeepBy p s = 
        let s' = dropWhile (not . p) s
        in if null s' 
            then []
            else 
                let (token, rest) = span p s'
                in token : splitKeepBy p rest
