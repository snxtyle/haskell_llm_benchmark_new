module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt s = 
    case mapM charToInt s of
        Just digits -> foldl (\acc x -> acc * 16 + x) 0 digits
        Nothing -> 0

charToInt :: Char -> Maybe Int
charToInt c
    | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
    | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
    | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
    | otherwise = Nothing
