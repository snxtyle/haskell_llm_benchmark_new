module Octal (readOct, showOct) where

-- Convert an octal string to its decimal Int value.
-- Return 0 for any invalid input (non-octal characters or empty string handling is fine: "" -> 0).
readOct :: String -> Int
readOct s =
  case traverse octDigit s of
    Nothing -> 0
    Just ds -> go 0 ds
  where
    -- Strict left fold: acc = acc * 8 + d
    go :: Int -> [Int] -> Int
    go acc [] = acc
    go acc (d:rest) =
      let acc' = acc * 8 + d
      in acc' `seq` go acc' rest

    -- Convert a character '0'..'7' to its Int value, otherwise Nothing
    octDigit :: Char -> Maybe Int
    octDigit c
      | c >= '0' && c <= '7' = Just (fromEnum c - fromEnum '0')
      | otherwise            = Nothing

-- Convert a non-negative Int to its octal string representation.
-- For 0 return "0". For negative inputs, return "0" (treat as invalid).
showOct :: Int -> String
showOct n
  | n < 0     = "0"
  | n == 0    = "0"
  | otherwise = reverse (toDigits n)
  where
    toDigits :: Int -> [Char]
    toDigits x
      | x == 0   = []
      | otherwise =
          let (q, r) = x `quotRem` 8
              c = toChar r
          in c : toDigits q

    toChar :: Int -> Char
    toChar d = toEnum (fromEnum '0' + d)
