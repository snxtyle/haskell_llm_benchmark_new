module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp capacity (length items)
  where
    -- Create a dynamic programming table
    dp w i
      | w <= 0 || i <= 0 = 0
      | weight > w = dp w (i - 1)
      | otherwise = max (dp w (i - 1)) (value + dp (w - weight) (i - 1))
      where
        (weight, value) = items !! (i - 1)
