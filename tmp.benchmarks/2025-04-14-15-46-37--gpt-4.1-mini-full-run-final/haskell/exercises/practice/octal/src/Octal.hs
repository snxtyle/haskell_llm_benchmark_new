module Octal (readOct, showOct) where

-- Convert a single octal digit character to its integer value.
-- Returns Nothing if the character is not a valid octal digit.
digitToInt :: Char -> Maybe Int
digitToInt c
  | c >= '0' && c <= '7' = Just (fromEnum c - fromEnum '0')
  | otherwise            = Nothing

-- Convert an octal string to decimal integer.
-- Invalid input (any non-octal digit) results in 0.
readOct :: String -> Int
readOct s = go s 0
  where
    go [] acc = acc
    go (x:xs) acc = case digitToInt x of
      Just d  -> let acc' = acc * 8 + d
                 in acc' `seq` go xs acc'
      Nothing -> 0

-- Convert a decimal integer to an octal string.
-- For zero, returns "0".
showOct :: Int -> String
showOct n
  | n <= 0    = "0"
  | otherwise = reverse (go n)
  where
    go 0 = []
    go x = let (q, r) = x `quotRem` 8
               c = toEnum (r + fromEnum '0') :: Char
           in c : go q
