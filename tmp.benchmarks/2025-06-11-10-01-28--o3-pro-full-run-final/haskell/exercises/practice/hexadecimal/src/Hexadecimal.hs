module Hexadecimal (hexToInt) where

import Data.Char (ord)

-- | Convert a hexadecimal string (e.g. "1a3f") to its decimal
--   value. If the input is empty or contains any non-hexadecimal
--   characters the function returns 0.
hexToInt :: String -> Int
hexToInt = go 0
  where
    -- Accumulator-based loop to avoid intermediate lists.
    go :: Int -> String -> Int
    go acc []     = acc
    go acc (c:cs) =
      case charToDigit c of
        Just d  -> go (acc * 16 + d) cs
        Nothing -> 0  -- Any invalid character makes the result 0.

    -- Convert a single hexadecimal character to its numeric value.
    charToDigit :: Char -> Maybe Int
    charToDigit c
      | '0' <= c && c <= '9' = Just (ord c - ord '0')
      | 'a' <= c && c <= 'f' = Just (10 + ord c - ord 'a')
      | 'A' <= c && c <= 'F' = Just (10 + ord c - ord 'A')
      | otherwise            = Nothing
