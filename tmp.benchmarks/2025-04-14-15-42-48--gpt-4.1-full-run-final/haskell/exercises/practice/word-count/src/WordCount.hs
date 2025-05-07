module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)
import qualified Data.Map.Strict as Map

-- | Split a string into words according to the rules:
--   - Words are separated by any punctuation or whitespace except apostrophes within words.
--   - Words are case-insensitive.
--   - Numbers are considered words.
--   - Apostrophes only allowed within words (e.g., contractions).
splitWords :: String -> [String]
splitWords = go []
  where
    go acc [] = reverse acc
    go acc s =
      let (word, rest) = nextWord s
      in if null word
         then go acc (dropWhile (not . isWordChar) rest)
         else go (map toLower word : acc) rest

    -- A word is a sequence of isAlphaNum or apostrophe (but apostrophe must be surrounded by letters)
    nextWord :: String -> (String, String)
    nextWord [] = ([], [])
    nextWord (c:cs)
      | isWordChar c =
          let (w, r) = spanWord c cs
          in (w, r)
      | otherwise = ([], cs)

    isWordChar c = isAlphaNum c || c == '\''

    -- Accepts a starting char (already checked to be isWordChar)
    spanWord :: Char -> String -> (String, String)
    spanWord c cs = go [c] cs
      where
        go w [] = (reverse w, [])
        go w (x:xs)
          | isAlphaNum x = go (x:w) xs
          | x == '\'' =
              -- Only allow apostrophe if surrounded by letters
              case w of
                (prev:_) | isAlpha prev ->
                  case xs of
                    (next:_) | isAlpha next -> go (x:w) xs
                    _ -> (reverse w, x:xs)
                _ -> (reverse w, x:xs)
          | otherwise = (reverse w, x:xs)
        isAlpha = (`elem` ['a'..'z'] ++ ['A'..'Z'])

wordCount :: String -> [(String, Int)]
wordCount xs =
  let ws = splitWords xs
      counts = Map.fromListWith (+) [(w, 1) | w <- ws]
  in Map.toList counts
