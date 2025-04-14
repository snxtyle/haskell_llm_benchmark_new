module Binary (toDecimal) where

toDecimal :: String -> Integer
toDecimal xs
  | null xs = 0  -- Handle empty string as invalid
  | not (all (`elem` "01") xs) = 0  -- Invalid input
  | otherwise = sum [if c == '1' then 2^i else 0 | (c, i) <- zip (reverse xs) [0..]]
