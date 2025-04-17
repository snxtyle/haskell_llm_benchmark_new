module Hexadecimal (hexToInt) where

-- | Convert a hexadecimal string to its decimal (base‑10) value.
--   Any invalid hexadecimal character causes the result to be 0.
hexToInt :: String -> Int
hexToInt = go 0
  where
    go :: Int -> String -> Int
    go acc []       = acc
    go acc (c : cs) =
      case charToDigit c of
        Just d  -> go (acc * 16 + d) cs
        Nothing -> 0  -- invalid character: short‑circuit to 0

-- | Convert a single hexadecimal character to its numeric value.
--   Returns Nothing for invalid characters.
charToDigit :: Char -> Maybe Int
charToDigit ch
  | '0' <= ch && ch <= '9' = Just $ fromEnum ch - fromEnum '0'
  | 'a' <= ch && ch <= 'f' = Just $ 10 + fromEnum ch - fromEnum 'a'
  | 'A' <= ch && ch <= 'F' = Just $ 10 + fromEnum ch - fromEnum 'A'
  | otherwise              = Nothing
