module Triangle (rows) where

-- | Compute Pascal's triangle up to a given number of rows.
-- Each number is computed by adding the numbers to the right and left
-- of the current position in the previous row.
rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n $ iterate nextRow [1]
  where
    -- Given a row, compute the next row in the triangle.
    -- e.g., nextRow [1, 3, 3, 1] -> [1, 4, 6, 4, 1]
    nextRow :: [Integer] -> [Integer]
    nextRow row = zipWith (+) (0 : row) (row ++ [0])
