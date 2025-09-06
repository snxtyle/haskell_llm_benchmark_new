module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = map toUpper . concatMap acronymFromWord . words . normalize
  where
    normalize :: String -> String
    normalize = map (\c -> if c == '-' then ' ' else c)
    
    acronymFromWord :: String -> String
    acronymFromWord "" = ""
    acronymFromWord (x:xs)
        | isAlpha x = x : filter isUpper xs
        | otherwise = acronymFromWord xs
