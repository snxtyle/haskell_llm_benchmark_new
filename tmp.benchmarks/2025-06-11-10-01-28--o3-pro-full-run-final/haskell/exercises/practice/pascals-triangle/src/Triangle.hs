module Triangle (rows) where

-- | Compute Pascal's triangle up to the given number of rows.
--   For n <= 0 we return an empty list.
--
--   Example:
--   rows 5
--   == [ [1]
--      , [1,1]
--      , [1,2,1]
--      , [1,3,3,1]
--      , [1,4,6,4,1]
--      ]
rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n $ iterate nextRow [1]
  where
    -- Given a row, produce the next row in Pascal's triangle.
    -- We pad the row with zeros on both ends and add pair-wise.
    nextRow :: [Integer] -> [Integer]
    nextRow row = zipWith (+) (0 : row) (row ++ [0])
