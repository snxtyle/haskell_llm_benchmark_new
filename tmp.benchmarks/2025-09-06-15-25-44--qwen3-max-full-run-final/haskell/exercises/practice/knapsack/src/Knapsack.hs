module Knapsack (maximumValue) where

import Data.Array

-- | Calculate the maximum value that can be carried in a knapsack
-- with given maximum weight capacity, using the provided items
-- Each item is represented as (weight, value)
-- Bob can take at most one of each item
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxWeight items = dp ! (itemCount, maxWeight)
  where
    itemCount = length items
    -- Create a memoization table using arrays
    dp = array ((0, 0), (itemCount, maxWeight)) 
         [((i, w), bestValue i w) | i <- [0..itemCount], w <- [0..maxWeight]]
    
    -- Get weight and value of item at index i (1-indexed)
    weight i = fst (items !! (i-1))
    value i = snd (items !! (i-1))
    
    -- Calculate the best value for first i items with weight limit w
    bestValue 0 _ = 0  -- No items means zero value
    bestValue i 0 = 0  -- Zero weight capacity means zero value
    bestValue i w
      | weight i > w = dp ! (i-1, w)  -- Can't include this item, too heavy
      | otherwise = max (dp ! (i-1, w))  -- Don't include item i
                      (dp ! (i-1, w - weight i) + value i)  -- Include item i
