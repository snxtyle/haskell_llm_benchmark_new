module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt hexString =
  case sequence (map digitToInt hexString) of
    Nothing -> 0
    Just digits -> foldl (\acc d -> acc * 16 + d) 0 digits

digitToInt :: Char -> Maybe Int
digitToInt c
  | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
  | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
  | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
  | otherwise = Nothing
