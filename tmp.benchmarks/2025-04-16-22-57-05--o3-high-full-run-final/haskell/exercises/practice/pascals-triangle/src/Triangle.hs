module Triangle (rows) where

-- | Compute the first n rows of Pascal's triangle.
--   For n <= 0 the result is the empty list.
--
--   Examples:
--
--   >>> rows 0
--   []
--
--   >>> rows 3
--   [[1],[1,1],[1,2,1]]
--
--   >>> rows 5
--   [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1]]
rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n pascal
  where
    pascal :: [[Integer]]
    pascal = iterate nextRow [1]

    -- Given a row, build the next one by zipping shifted copies
    nextRow :: [Integer] -> [Integer]
    nextRow row = zipWith (+) (0 : row) (row ++ [0])
