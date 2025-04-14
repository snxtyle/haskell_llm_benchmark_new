module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | n == 1    = [[1]]
  | otherwise = [1] : map nextRow (rows (n-1))
  where
    nextRow row = zipWith (+) (0:row) (row ++ [0])
