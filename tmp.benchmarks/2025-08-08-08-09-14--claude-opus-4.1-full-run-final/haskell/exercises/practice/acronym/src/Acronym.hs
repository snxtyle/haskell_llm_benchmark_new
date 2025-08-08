module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toLower, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ concatMap firstLetter (words' xs)
  where
    -- Get first letter of a word (if it exists)
    firstLetter :: String -> String
    firstLetter [] = []
    firstLetter (c:_) = [c]
    
    -- Split on whitespace and hyphens, then handle camelCase
    words' :: String -> [String]
    words' s = concatMap splitCamelCase $ wordsBy isWordSeparator s
    
    -- Check if character is a word separator (whitespace or hyphen)
    isWordSeparator :: Char -> Bool
    isWordSeparator c = c == ' ' || c == '-' || c == '\t' || c == '\n'
    
    -- Split string by a predicate, keeping only alphabetic characters in each word
    wordsBy :: (Char -> Bool) -> String -> [String]
    wordsBy _ [] = []
    wordsBy p s = case dropWhile p s of
                    [] -> []
                    s' -> let (word, rest) = break p s'
                              cleanWord = filter isAlpha word
                          in (if null cleanWord then [] else [cleanWord]) ++ wordsBy p rest
    
    -- Split camelCase words into separate words
    splitCamelCase :: String -> [String]
    splitCamelCase [] = []
    splitCamelCase word@(c:cs)
      | all isUpper word = [word]  -- All uppercase like "HTML"
      | otherwise = case splitOnUppercase word of
                      [] -> []
                      [w] -> [w]
                      ws -> ws
    
    -- Split a word on uppercase letters (for camelCase)
    splitOnUppercase :: String -> [String]
    splitOnUppercase [] = []
    splitOnUppercase (c:cs)
      | isUpper c = case cs of
                      [] -> [[c]]
                      (next:rest) | isUpper next -> 
                        -- Multiple uppercase letters in a row
                        let (uppers, remainder) = span isUpper cs
                        in if null remainder
                           then [c:uppers]  -- All uppercase to end
                           else [c:init uppers] ++ splitOnUppercase (last uppers:remainder)
                      _ -> [c] : splitOnUppercase cs
      | otherwise = case break isUpper (c:cs) of
                      (word, []) -> [word]
                      (word, rest) -> word : splitOnUppercase rest
