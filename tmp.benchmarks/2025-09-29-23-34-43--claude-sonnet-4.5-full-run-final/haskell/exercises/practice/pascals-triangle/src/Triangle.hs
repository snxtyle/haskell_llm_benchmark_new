module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x
  | x < 0     = []
  | x == 0    = []
  | otherwise = take x pascalTriangle

pascalTriangle :: [[Integer]]
pascalTriangle = iterate nextRow [1]

nextRow :: [Integer] -> [Integer]
nextRow prev = 1 : zipWith (+) prev (tail prev) ++ [1]
