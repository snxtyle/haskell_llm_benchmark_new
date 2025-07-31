module Hexadecimal (hexToInt) where

import Data.Char (ord)

-- Convert a hexadecimal string to a decimal integer.
-- Returns 0 if the input contains any invalid hex character.
hexToInt :: String -> Int
hexToInt = go 0
  where
    go acc [] = acc
    go acc (c:cs) =
      case hexVal c of
        Nothing   -> 0
        Just digit -> go (acc * 16 + digit) cs

-- Convert a single hexadecimal character to its numeric value.
-- Supports '0'-'9', 'a'-'f', and 'A'-'F'.
hexVal :: Char -> Maybe Int
hexVal c
  | c >= '0' && c <= '9' = Just (ord c - ord '0')
  | c >= 'a' && c <= 'f' = Just (10 + ord c - ord 'a')
  | c >= 'A' && c <= 'F' = Just (10 + ord c - ord 'A')
  | otherwise            = Nothing
