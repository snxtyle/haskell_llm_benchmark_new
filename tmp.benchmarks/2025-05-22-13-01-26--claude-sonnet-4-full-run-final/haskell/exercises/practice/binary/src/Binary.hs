module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
  | null xs = 0
  | not (all (`elem` "01") xs) = 0
  | otherwise = sum $ zipWith (*) (map digitToInt (reverse xs)) (map (2^) [0::Int..])
  where
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt _   = 0
