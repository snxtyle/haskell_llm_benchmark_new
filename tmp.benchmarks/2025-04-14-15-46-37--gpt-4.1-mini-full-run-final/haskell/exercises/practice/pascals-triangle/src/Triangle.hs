module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n $ iterate nextRow [1]
  where
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
