module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toLower, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ concatMap extractFirstLetters $ splitWords xs
  where
    -- Split input into words, treating hyphens and spaces as separators
    splitWords :: String -> [String]
    splitWords [] = []
    splitWords s = case dropWhile isSeparator s of
      [] -> []
      s' -> let (word, rest) = break isSeparator s'
            in word : splitWords rest
    
    -- Check if character is a word separator (hyphen or space)
    isSeparator :: Char -> Bool
    isSeparator c = c == '-' || c == ' '
    
    -- Extract first letters from a word
    -- For regular words: take the first alphabetic character
    -- For camelCase: take the first letter and all subsequent uppercase letters
    extractFirstLetters :: String -> String
    extractFirstLetters word =
      let cleaned = filter isAlpha word
      in case cleaned of
        [] -> ""
        (c:cs) -> c : filter isUpper cs
