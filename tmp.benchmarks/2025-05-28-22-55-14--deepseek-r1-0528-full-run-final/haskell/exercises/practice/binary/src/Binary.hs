module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal s
  | all (\c -> c == '0' || c == '1') s = foldl (\acc c -> 2 * acc + if c == '1' then 1 else 0) 0 s
  | otherwise = 0
