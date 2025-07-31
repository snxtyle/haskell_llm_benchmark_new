module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n pascalTriangle

pascalTriangle :: [[Integer]]
pascalTriangle = [1] : map nextRow pascalTriangle
  where
    nextRow row = zipWith (+) (0 : row) (row ++ [0])
