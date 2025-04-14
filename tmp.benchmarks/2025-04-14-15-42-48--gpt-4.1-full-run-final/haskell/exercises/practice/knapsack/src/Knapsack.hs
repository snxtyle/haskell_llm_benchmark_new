module Knapsack (maximumValue) where

-- | maximumValue n items
--   n: maximum weight the knapsack can carry
--   items: list of (weight, value) pairs
--   returns: the maximum value that can be carried
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = knapsack capacity (length items)
  where
    -- Use memoization via a table for efficiency
    -- dp[i][w] = max value using first i items with capacity w
    knapsack :: Int -> Int -> Int
    knapsack w 0 = 0
    knapsack w i
      | w <= 0 = 0
      | itemWeight > w = knapsack w (i-1)
      | otherwise = max (knapsack w (i-1))
                        (itemValue + knapsack (w - itemWeight) (i-1))
      where
        (itemWeight, itemValue) = items !! (i-1)
