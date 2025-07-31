module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n $ iterate nextRow [1]
  where
    nextRow :: [Integer] -> [Integer]
    nextRow prev = zipWith (+) ([0] ++ prev) (prev ++ [0])
