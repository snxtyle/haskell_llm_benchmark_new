module Triangle (rows) where

-- | Compute the first n rows of Pascal's triangle.
--   rows 0 == []
--   rows 1 == [[1]]
--   rows 5 == [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
rows :: Int -> [[Integer]]
rows n = take n pascal
  where
    pascal :: [[Integer]]
    pascal = iterate next [1]

    next :: [Integer] -> [Integer]
    next xs = zipWith (+) (0 : xs) (xs ++ [0])
