module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)

wordCount :: String -> [(String, Int)]
wordCount xs = foldr insertWord [] (filter (not . null) words)
  where
    words = map (map toLower) $ tokenize xs
    
    -- Insert word into accumulator, incrementing count if already present
    insertWord word [] = [(word, 1)]
    insertWord word ((w, count):rest)
      | word == w = (w, count + 1) : rest
      | otherwise = (w, count) : insertWord word rest

-- Tokenize the string into words
tokenize :: String -> [String]
tokenize [] = []
tokenize xs = 
  let (word, rest) = span isWordChar xs
      (_, rest') = span (not . isWordChar) rest
  in word : tokenize rest'
  where
    isWordChar c = isAlphaNum c || (c == '\'' && not (isQuoteContext xs))
    
-- Check if the apostrophe is in a quote context (surrounded by letters)
isQuoteContext :: String -> Bool
isQuoteContext [] = False
isQuoteContext (_:[]) = False
isQuoteContext (prev:current:rest)
  | current == '\'' = 
      if null rest then False
      else isAlphaNum prev && isAlphaNum (head rest)
  | otherwise = isQuoteContext (current:rest)
