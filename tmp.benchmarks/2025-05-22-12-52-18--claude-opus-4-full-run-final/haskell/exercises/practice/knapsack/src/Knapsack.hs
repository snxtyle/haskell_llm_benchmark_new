module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dp capacity items
  where
    dp :: Int -> [(Int, Int)] -> Int
    dp maxWeight itemList = 
      let n = length itemList
          itemsArray = zip [1..] itemList
          
          -- Create a memoization table using list comprehension
          -- dpTable[i][w] represents max value using first i items with weight limit w
          dpTable = [[computeValue i w | w <- [0..maxWeight]] | i <- [0..n]]
          
          computeValue :: Int -> Int -> Int
          computeValue 0 _ = 0  -- No items means 0 value
          computeValue _ 0 = 0  -- No capacity means 0 value
          computeValue i w =
            let (weight, value) = itemList !! (i - 1)
                withoutItem = dpTable !! (i - 1) !! w
                withItem = if weight <= w 
                          then value + (dpTable !! (i - 1) !! (w - weight))
                          else 0
            in max withoutItem withItem
            
      in dpTable !! n !! maxWeight
