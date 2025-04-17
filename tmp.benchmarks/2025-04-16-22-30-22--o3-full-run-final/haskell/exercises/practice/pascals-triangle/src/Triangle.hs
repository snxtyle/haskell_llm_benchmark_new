module Triangle (rows) where

-- | Generate Pascal's triangle up to the given number of rows.
--   For non‑positive input we return an empty list.
rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n $ iterate nextRow [1]
  where
    -- Build the next row by padding the current row with zeros on both ends
    -- and summing adjacent elements.
    nextRow :: [Integer] -> [Integer]
    nextRow r = zipWith (+) (0 : r) (r ++ [0])
