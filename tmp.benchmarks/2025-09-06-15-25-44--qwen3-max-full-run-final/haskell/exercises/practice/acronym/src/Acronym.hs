module Acronym (abbreviate) where

import Data.Char (isLetter, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ filter isLetter $ concatMap firstLetters $ wordsWhen (\c -> c == ' ' || c == '-') xs
  where
    firstLetters :: String -> String
    firstLetters [] = []
    firstLetters (x:xs) = x : map snd (filter (isUpper . fst) (zip xs xs))
    
    wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> let (w, rest) = break p s' in w : wordsWhen p rest
