module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n (iterate next [1])
  where
    next :: [Integer] -> [Integer]
    next xs = zipWith (+) (0 : xs) (xs ++ [0])
