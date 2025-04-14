module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt str = 
    if allValidChars str
        then foldl (\acc c -> acc * 16 + charToInt c) 0 (map toLower str)
        else 0
  where
    allValidChars = all (\c -> elem (toLower c) "0123456789abcdef")
    charToInt c
        | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
        | otherwise = 0
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c
