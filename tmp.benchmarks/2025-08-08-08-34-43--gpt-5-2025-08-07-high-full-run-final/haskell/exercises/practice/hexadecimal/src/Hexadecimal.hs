module Hexadecimal (hexToInt) where

-- | Convert a hexadecimal string to its decimal Int.
--   Returns 0 if the input contains any invalid hexadecimal characters.
hexToInt :: String -> Int
hexToInt = go 0
  where
    go acc [] = acc
    go acc (c:cs) =
      case charVal c of
        Just v  -> go (acc * 16 + v) cs
        Nothing -> 0

charVal :: Char -> Maybe Int
charVal c
  | '0' <= c && c <= '9' = Just (fromEnum c - fromEnum '0')
  | 'a' <= c && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
  | otherwise            = Nothing
