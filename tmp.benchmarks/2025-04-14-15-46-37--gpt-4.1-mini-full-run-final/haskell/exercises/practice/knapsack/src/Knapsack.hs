module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = knapsack capacity items
  where
    knapsack 0 _ = 0
    knapsack _ [] = 0
    knapsack w ((weight, value):xs)
      | weight > w = knapsack w xs
      | otherwise = max (value + knapsack (w - weight) xs) (knapsack w xs)
