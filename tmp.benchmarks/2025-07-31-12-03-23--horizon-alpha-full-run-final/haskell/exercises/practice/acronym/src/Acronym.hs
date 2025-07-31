module Acronym (abbreviate) where

import Data.Char (isLetter, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = map toUpper . concatMap acronymParts . tokenize
  where
    -- Split the input into letter-only tokens; hyphens act like spaces,
    -- other punctuation is removed (treated as separators).
    tokenize :: String -> [String]
    tokenize = words . map toWordSep

    toWordSep :: Char -> Char
    toWordSep c
      | c == '-'        = ' '
      | isLetter c      = c
      | otherwise       = ' '

    -- For a token, take the first character and any internal uppercase letters
    -- that denote camelCase boundaries.
    acronymParts :: String -> String
    acronymParts []     = []
    acronymParts (x:xs) = x : camelCaps x xs

    camelCaps :: Char -> String -> String
    camelCaps _ [] = []
    camelCaps prev (c:cs)
      | isUpper c && not (isUpper prev) = c : camelCaps c cs
      | otherwise                       = camelCaps c cs
