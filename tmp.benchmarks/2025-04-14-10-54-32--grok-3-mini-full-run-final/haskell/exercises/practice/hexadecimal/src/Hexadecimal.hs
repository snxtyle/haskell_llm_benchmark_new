module Hexadecimal (hexToInt) where

isHexDigit :: Char -> Bool
isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

digitValue :: Char -> Int
digitValue c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'  -- Convert '0'-'9' to 0-9
  | c >= 'a' && c <= 'f' = 10 + (fromEnum c - fromEnum 'a')  -- Convert 'a'-'f' to 10-15
  | c >= 'A' && c <= 'F' = 10 + (fromEnum c - fromEnum 'A')  -- Convert 'A'-'F' to 10-15
  | otherwise = 0  -- This won't be called directly, as we check validity first

hexToInt :: String -> Int
hexToInt s
  | all isHexDigit s = foldl (\acc (c, i) -> acc + digitValue c * 16^i) 0 (zip (reverse s) [0..])  -- Convert valid string
  | otherwise = 0  -- Return 0 for invalid strings
