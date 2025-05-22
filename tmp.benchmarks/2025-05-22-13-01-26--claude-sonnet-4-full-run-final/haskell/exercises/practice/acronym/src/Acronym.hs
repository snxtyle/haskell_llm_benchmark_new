module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate = map toUpper . map head . words . addSpaces
  where
    addSpaces [] = []
    addSpaces [c] = [processChar c]
    addSpaces (c1:c2:cs)
      | c1 == '-' = ' ' : addSpaces (c2:cs)
      | not (isAlpha c1) = ' ' : addSpaces (c2:cs)
      | isAlpha c1 && isAlpha c2 && not (isUpper c1) && isUpper c2 = 
          c1 : ' ' : addSpaces (c2:cs)
      | otherwise = c1 : addSpaces (c2:cs)
    
    processChar c
      | isAlpha c = c
      | otherwise = ' '
