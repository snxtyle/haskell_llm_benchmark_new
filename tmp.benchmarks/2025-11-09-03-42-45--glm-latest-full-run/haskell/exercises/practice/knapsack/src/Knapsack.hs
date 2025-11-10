module Knapsack (maximumValue) where

import Data.Array
import Data.List (foldl')

-- | Calculates the maximum value of items that can fit in a knapsack.
-- This is the classic 0/1 knapsack problem solved using dynamic programming.
--
-- The first argument is the maximum weight capacity of the knapsack.
-- The second argument is a list of items, where each item is a tuple
-- (weight, value).
--
-- The function returns the maximum total value achievable.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = finalDp ! capacity
  where
    -- Initial DP array: all zeros for capacities 0 to 'capacity'.
    -- dp ! w represents the maximum value achievable with capacity w.
    initialDp :: Array Int Int
    initialDp = listArray (0, capacity) (replicate (capacity + 1) 0)

    -- Fold over the items, updating the DP array for each one.
    -- 'foldl'' is used for strictness to prevent stack overflows.
    finalDp :: Array Int Int
    finalDp = foldl' updateDp initialDp items

    -- Update the DP array for a single item.
    -- This function takes the current DP array and an item, and returns
    -- a new DP array that reflects the consideration of the new item.
    updateDp :: Array Int Int -> (Int, Int) -> Array Int Int
    updateDp dp (weight, value) =
      -- Create a new array with the updated values.
      array (0, capacity) [(w, newValue w) | w <- [0..capacity]]
      where
        -- Calculate the new value for a given capacity 'w'.
        newValue :: Int -> Int
        newValue w
          -- If the current item is too heavy for capacity 'w',
          -- the value is the same as it was without this item.
          | w < weight = dp ! w
          -- Otherwise, it's the maximum of two choices:
          -- 1. Not taking the item (dp ! w)
          -- 2. Taking the item (value + dp ! (w - weight))
          | otherwise  = max (dp ! w) (value + dp ! (w - weight))
