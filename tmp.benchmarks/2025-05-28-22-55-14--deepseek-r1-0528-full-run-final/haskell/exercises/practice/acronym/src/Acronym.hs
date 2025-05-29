module Acronym (abbreviate) where

import Data.Char (isAlpha, toUpper, isLower, isUpper)

abbreviate :: String -> String
abbreviate s = 
    let s1 = map (\c -> if c == '-' then ' ' else c) s
        s2 = filter (\c -> isAlpha c || c == ' ') s1
        splitCamel [] = []
        splitCamel (x:xs) = x : go x xs
            where
                go _ [] = []
                go prev (y:ys)
                    | isLower prev && isUpper y = ' ' : y : go y ys
                    | otherwise = y : go y ys
        s3 = splitCamel s2
        wordsList = words s3
    in map (toUpper . head) wordsList
