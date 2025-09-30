module Hexadecimal (hexToInt) where

import Data.Char (toLower)

hexToInt :: String -> Int
hexToInt "" = 0
hexToInt str = 
  if all isValidHexChar str
    then convertHex str
    else 0
  where
    isValidHexChar c = c `elem` "0123456789abcdefABCDEF"
    
    convertHex = foldl (\acc c -> acc * 16 + charToInt c) 0
    
    charToInt c = case toLower c of
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
      _   -> 0  -- This should never happen due to validation
