module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ acronymWords xs

acronymWords :: String -> String
acronymWords [] = []
acronymWords (c:cs)
  | isAlpha c = c : go c cs
  | c == '-'  = go ' ' cs
  | otherwise = go ' ' cs
  where
    go _ [] = []
    go prev (x:xs)
      | x == '-' = go ' ' xs
      | isAlpha x && (not (isAlpha prev) || (isLower prev && isUpper x)) = x : go x xs
      | otherwise = go x xs
    isLower ch = ch >= 'a' && ch <= 'z'
