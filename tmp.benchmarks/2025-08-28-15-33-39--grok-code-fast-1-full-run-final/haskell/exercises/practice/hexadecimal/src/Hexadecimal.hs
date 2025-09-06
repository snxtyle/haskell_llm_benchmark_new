module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt s
  | null s = error "invalid hexadecimal string"
  | all isValidHex s = foldl (\acc c -> acc * 16 + digitValue c) 0 s
  | otherwise = error "invalid hexadecimal string"

isValidHex :: Char -> Bool
isValidHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

digitValue :: Char -> Int
digitValue c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
  | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
  | otherwise = error "Invalid hex digit"  -- This should not happen due to prior validation
