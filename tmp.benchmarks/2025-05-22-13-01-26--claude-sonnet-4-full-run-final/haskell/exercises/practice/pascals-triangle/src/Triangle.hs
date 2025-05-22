module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n pascalTriangle
  where
    pascalTriangle = [1] : map nextRow pascalTriangle
    nextRow row = zipWith (+) (0 : row) (row ++ [0])
