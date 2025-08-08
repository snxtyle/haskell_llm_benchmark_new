module Hexadecimal (hexToInt) where

import Data.Char (ord, toLower)

hexToInt :: String -> Int
hexToInt = go 0
  where
    go :: Int -> String -> Int
    go acc [] = acc
    go acc (c:cs) =
      case hexValue c of
        Just v  -> go (acc * 16 + v) cs
        Nothing -> 0

hexValue :: Char -> Maybe Int
hexValue c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= cl && cl <= 'f' = Just (10 + ord cl - ord 'a')
  | otherwise = Nothing
  where
    cl = toLower c
