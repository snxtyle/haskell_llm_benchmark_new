module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n $ iterate nextRow [1]
  where
    nextRow row = zipWith (+) (0:row) (row ++ [0])
