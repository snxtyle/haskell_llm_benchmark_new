module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp numItems capacity
  where
    numItems = length items
    itemArray = zip [1..] items  -- Index items from 1
    
    -- dp i w = maximum value using first i items with capacity w
    dp :: Int -> Int -> Int
    dp 0 _ = 0  -- No items, value is 0
    dp _ 0 = 0  -- No capacity, value is 0
    dp i w
      | w < 0 = 0  -- Invalid capacity
      | otherwise = 
          let (weight, value) = snd (itemArray !! (i - 1))
              withoutItem = dp (i - 1) w
              withItem = if weight <= w
                         then value + dp (i - 1) (w - weight)
                         else 0
          in max withoutItem withItem
