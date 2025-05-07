module Triangle (rows) where

-- | Generates the next row of Pascal's triangle from the current row.
--   It achieves this by pairwise summing elements of the row padded with 0s.
--   Example: nextRow [1, 2, 1] == [1, 3, 3, 1]
--   Calculation: zipWith (+) [0, 1, 2, 1] [1, 2, 1, 0] == [1, 3, 3, 1]
nextRow :: [Integer] -> [Integer]
nextRow r = zipWith (+) (0:r) (r ++ [0])

-- | Generates an infinite list representing all rows of Pascal's triangle.
pascalTriangle :: [[Integer]]
pascalTriangle = iterate nextRow [1]

-- | Computes the first n rows of Pascal's triangle.
rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []          -- No rows for non-positive input
  | otherwise = take n pascalTriangle -- Take the first n rows from the infinite triangle
