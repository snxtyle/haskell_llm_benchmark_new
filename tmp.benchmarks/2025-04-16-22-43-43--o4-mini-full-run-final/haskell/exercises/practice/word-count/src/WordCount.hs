module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map

wordCount :: String -> [(String, Int)]
wordCount xs = Map.toList counts
  where
    -- Split into raw tokens, normalize and filter out empties
    tokens = filter (not . null) $ map normalize $ split xs
    -- Build a frequency map
    counts = Map.fromListWith (+) [(t, 1) | t <- tokens]

-- Break the string into chunks of word characters
split :: String -> [String]
split [] = []
split s@(c:cs)
  | isWordChar c = let (w, rest) = span isWordChar s in w : split rest
  | otherwise    = split cs

-- Define what constitutes a word character
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\''

-- Normalize a token: strip outer apostrophes and lowercase
normalize :: String -> String
normalize = map toLower . stripApostrophes

-- Remove leading and trailing apostrophes
stripApostrophes :: String -> String
stripApostrophes = dropWhile (=='\'') . dropWhileEnd (=='\'')
