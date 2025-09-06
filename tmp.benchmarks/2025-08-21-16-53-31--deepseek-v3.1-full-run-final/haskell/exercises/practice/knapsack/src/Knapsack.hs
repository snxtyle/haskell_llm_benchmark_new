module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight items = knapsack maxWeight items
  where
    knapsack :: Int -> [(Int, Int)] -> Int
    knapsack capacity itemList
      | capacity <= 0 || null itemList = 0
      | otherwise = dp (length itemList) capacity
    
    dp :: Int -> Int -> Int
    dp i w
      | i == 0 || w == 0 = 0
      | weight > w = dp (i - 1) w
      | otherwise = max (dp (i - 1) w) (value + dp (i - 1) (w - weight))
      where
        (weight, value) = itemList !! (i - 1)
    
    itemList = items
