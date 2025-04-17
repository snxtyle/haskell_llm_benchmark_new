module Hexadecimal (hexToInt) where

import Data.Char (ord, toLower)
import Data.List (foldl')

-- | Convert a hexadecimal string to its decimal (base‑10) equivalent.
--   If the input contains any character that is not a valid hexadecimal
--   digit, the result is 0.
--
--   Examples:
--
--   >>> hexToInt "1"
--   1
--   >>> hexToInt "cafe"
--   51966
--   >>> hexToInt "carrot"
--   0
hexToInt :: String -> Int
hexToInt xs =
  case traverse charToDigit xs of
    Nothing      -> 0
    Just digits  -> foldl' (\acc d -> acc * 16 + d) 0 digits

-- Convert a single hexadecimal digit to its integer value.
-- Returns Nothing for invalid characters.
charToDigit :: Char -> Maybe Int
charToDigit c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= lc && lc <= 'f' = Just (10 + ord lc - ord 'a')
  | otherwise = Nothing
  where
    lc = toLower c
