module Acronym (abbreviate) where

import Data.Char (isLetter, isSpace, toUpper, isLower, isUpper)

abbreviate :: String -> String
abbreviate xs = map (toUpper . head) . filter (not . null) . words $ camelSplit (process xs)
  where process = map (\c -> if c == '-' then ' ' else c) . filter (\c -> isLetter c || isSpace c || c == '-')
        camelSplit [] = []
        camelSplit (c:cs) = c : camelSplit' c cs
        camelSplit' _ [] = []
        camelSplit' prev (d:ds) | isLower prev && isUpper d = ' ' : d : camelSplit' d ds
                                | otherwise = d : camelSplit' d ds
