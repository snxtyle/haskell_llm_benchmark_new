module Hexadecimal (hexToInt) where

import Data.Char (toLower)

hexToInt :: String -> Int
hexToInt "" = 0
hexToInt xs = if all isValidHexChar xs
              then foldl (\acc x -> acc * 16 + hexCharToInt x) 0 xs
              else 0
  where
    isValidHexChar c = c `elem` "0123456789abcdefABCDEF"
    
    hexCharToInt c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | toLower c >= 'a' && toLower c <= 'f' = fromEnum (toLower c) - fromEnum 'a' + 10
      | otherwise = 0  -- This case shouldn't occur due to validation above
