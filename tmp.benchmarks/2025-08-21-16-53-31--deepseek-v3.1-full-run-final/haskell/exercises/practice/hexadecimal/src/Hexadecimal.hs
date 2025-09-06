module Hexadecimal (hexToInt) where

import Data.Char (toLower)

hexToInt :: String -> Int
hexToInt s = case allValidHexChars s of
    True -> foldl (\acc c -> acc * 16 + hexDigitToInt c) 0 (map toLower s)
    False -> 0
  where
    allValidHexChars :: String -> Bool
    allValidHexChars = all isHexDigit
    
    isHexDigit :: Char -> Bool
    isHexDigit c = toLower c `elem` ['0'..'9'] ++ ['a'..'f']
    
    hexDigitToInt :: Char -> Int
    hexDigitToInt c = case toLower c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _   -> 0  -- This case should never happen due to allValidHexChars check
