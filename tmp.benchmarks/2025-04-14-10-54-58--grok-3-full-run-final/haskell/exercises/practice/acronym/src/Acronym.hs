module Acronym (abbreviate) where

abbreviate :: String -> String
abbreviate xs = map toUpper $ map head $ words $ replaceHyphen $ filterAlpha xs
  where
    filterAlpha = filter (\c -> c == '-' || c == ' ' || isAlpha c)
    replaceHyphen = map (\c -> if c == '-' then ' ' else c)
    isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
