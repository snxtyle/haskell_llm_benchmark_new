module Acronym (abbreviate) where

import Data.Char (isPunctuation, toUpper, isLower, isUpper)

abbreviate :: String -> String
abbreviate =
  map toUpper
  . concatMap initials
  . words
  . filter (not . isPunctuation)
  . map (\c -> if c == '-' then ' ' else c)
  where
    initials :: String -> String
    initials []     = []
    initials (x:xs) =
      x : [ c | (p, c) <- zip (x:xs) xs, isLower p && isUpper c ]
