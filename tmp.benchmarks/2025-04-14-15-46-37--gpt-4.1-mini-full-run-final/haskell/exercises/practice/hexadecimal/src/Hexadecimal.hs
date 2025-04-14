module Hexadecimal (hexToInt) where

import Data.Char (toLower)

hexToInt :: String -> Maybe Int
hexToInt s = go (reverse s) 0 0
  where
    go :: [Char] -> Int -> Int -> Maybe Int
    go [] _ acc = Just acc
    go (c:cs) pos acc =
      case hexDigitToInt c of
        Just val -> go cs (pos + 1) (acc + val * (16 ^ pos))
        Nothing  -> Nothing

    hexDigitToInt :: Char -> Maybe Int
    hexDigitToInt ch
      | '0' <= ch && ch <= '9' = Just (fromEnum ch - fromEnum '0')
      | 'a' <= lc && lc <= 'f' = Just (10 + fromEnum lc - fromEnum 'a')
      | otherwise              = Nothing
      where lc = toLower ch
