module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp capacity (length items)
  where
    dp :: Int -> Int -> Int
    dp w i
      | i == 0 = 0
      | weight > w = dp w (i-1)
      | otherwise = max (value + dp (w - weight) (i-1)) (dp w (i-1))
      where
        (value, weight) = items !! (i-1)
