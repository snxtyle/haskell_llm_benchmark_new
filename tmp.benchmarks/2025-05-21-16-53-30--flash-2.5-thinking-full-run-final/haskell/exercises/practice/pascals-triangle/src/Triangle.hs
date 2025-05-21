module Triangle (rows) where

-- | Generates Pascal's triangle up to a given number of rows.
--
-- Each number in Pascal's Triangle is computed by adding the numbers to the
-- right and left of the current position in the previous row.
--
-- Examples:
-- rows 0 == []
-- rows 1 == [[1]]
-- rows 2 == [[1], [1,1]]
-- rows 3 == [[1], [1,1], [1,2,1]]
rows :: Int -> [[Integer]]
rows n
    | n <= 0    = []
    | otherwise = take n $ iterate nextRow [1]
  where
    -- | Computes the next row of Pascal's triangle from the current row.
    --
    -- Given a row `[a, b, c, d]`, the next row is `[1, a+b, b+c, c+d, 1]`.
    nextRow :: [Integer] -> [Integer]
    nextRow prevRow = 1 : zipWith (+) prevRow (tail prevRow) ++ [1]
