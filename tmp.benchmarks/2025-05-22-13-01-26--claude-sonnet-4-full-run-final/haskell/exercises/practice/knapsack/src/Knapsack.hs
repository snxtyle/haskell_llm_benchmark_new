module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp capacity (length items)
  where
    itemArray = zip [0..] items
    
    dp w n
      | w <= 0 || n <= 0 = 0
      | weight > w = dp w (n-1)
      | otherwise = max (dp w (n-1)) (value + dp (w - weight) (n-1))
      where
        (weight, value) = items !! (n-1)

-- Alternative more efficient implementation using memoization
-- maximumValue :: Int -> [(Int, Int)] -> Int
-- maximumValue capacity items = 
--   let n = length items
--       bounds = ((0, 0), (n, capacity))
--       memo = array bounds [((i, w), solve i w) | i <- [0..n], w <- [0..capacity]]
--       
--       solve 0 _ = 0
--       solve _ 0 = 0
--       solve i w
--         | weight > w = memo ! (i-1, w)
--         | otherwise = max (memo ! (i-1, w)) (value + memo ! (i-1, w - weight))
--         where (weight, value) = items !! (i-1)
--   in memo ! (n, capacity)
