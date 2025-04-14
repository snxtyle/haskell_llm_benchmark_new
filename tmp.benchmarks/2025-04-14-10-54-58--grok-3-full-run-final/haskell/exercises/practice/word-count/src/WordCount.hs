module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum, isSpace)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = 
    let 
        -- Convert to lowercase for case-insensitive counting
        lowerStr = map toLower xs
        -- Split into words, handling contractions
        wordsList = splitIntoWords lowerStr
        -- Group same words together and count occurrences
        grouped = group $ sort wordsList
        -- Create tuples of (word, count)
        counted = map (\ws -> (head ws, length ws)) grouped
    in 
        counted
  where
    -- Split string into words, preserving apostrophes in contractions
    splitIntoWords :: String -> [String]
    splitIntoWords str = filter (not . null) $ foldr splitHelper [""] str
    
    splitHelper :: Char -> [String] -> [String]
    splitHelper c (current:rest)
        | isSpace c || (not (c == '\'') && not (isAlphaNum c)) = "":current:rest
        | otherwise = (c:current):rest
    splitHelper _ [] = []
