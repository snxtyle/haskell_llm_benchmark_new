module Acronym (abbreviate) where

import Data.Char (isLetter, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = map toUpper . map head . getWords . normalize
  where
    normalize = map replaceHyphen . filter (\c -> isLetter c || c == ' ' || c == '-')
    replaceHyphen '-' = ' '
    replaceHyphen c = c
    
    getWords [] = []
    getWords xs = case dropWhile (== ' ') xs of
      [] -> []
      ys -> let (word, rest) = breakWord ys
            in word : getWords rest
    
    breakWord [] = ([], [])
    breakWord (x:xs)
      | x == ' ' = ([], xs)
      | otherwise = let (word, rest) = breakWord' xs
                    in (x:word, rest)
    
    breakWord' [] = ([], [])
    breakWord' (x:xs)
      | x == ' ' = ([], xs)
      | isUpper x = ([], x:xs)  -- Start of new word in camelCase
      | otherwise = let (word, rest) = breakWord' xs
                    in (x:word, rest)
