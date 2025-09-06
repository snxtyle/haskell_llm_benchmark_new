module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = sum $ zipWith (*) (reverse $ map digitToInt xs) (map (2^) [0..])
  | otherwise = 0
  where
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt _ = 0  -- This shouldn't be reached due to the guard above
