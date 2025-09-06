module WordCount (wordCount) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount = countWords . extractWords

extractWords :: String -> [String]
extractWords = filter (not . null) . splitWords

splitWords :: String -> [String]
splitWords = splitWords' [] []
  where
    splitWords' current acc [] = 
        if null current then reverse acc else reverse (reverse current : acc)
    splitWords' current acc (c:cs)
        | isAlphaNum c = splitWords' (toLower c : current) acc cs
        | c == '\'' = splitWords' (toLower c : current) acc cs
        | otherwise = 
            if null current 
                then splitWords' [] acc cs
                else splitWords' [] (reverse current : acc) cs

countWords :: [String] -> [(String, Int)]
countWords = map (\ws -> (head ws, length ws)) . group . sort
