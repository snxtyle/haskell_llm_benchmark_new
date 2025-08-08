module Knapsack (maximumValue) where

import Data.Array

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = knapsack capacity items
  where
    knapsack :: Int -> [(Int, Int)] -> Int
    knapsack maxWeight weightValuePairs = 
      let n = length weightValuePairs
          itemsArray = listArray (1, n) weightValuePairs
          
          -- Create a DP table where dp ! (i, w) represents 
          -- max value using first i items with weight limit w
          dp = array ((0, 0), (n, maxWeight)) 
               [((i, w), dpValue i w) | i <- [0..n], w <- [0..maxWeight]]
          
          dpValue 0 _ = 0  -- No items means 0 value
          dpValue _ 0 = 0  -- No capacity means 0 value
          dpValue i w = 
            let (weight, value) = itemsArray ! i
            in if weight > w
               then dp ! (i - 1, w)  -- Can't include this item
               else max (dp ! (i - 1, w))  -- Don't include
                        (dp ! (i - 1, w - weight) + value)  -- Include
          
      in dp ! (n, maxWeight)
