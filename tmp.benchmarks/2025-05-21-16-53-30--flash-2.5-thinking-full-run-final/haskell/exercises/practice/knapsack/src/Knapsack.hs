module Knapsack (maximumValue) where

import Data.Array (Array, array, (!))

-- | Calculates the maximum value that can be obtained from a list of items
--   given a knapsack's maximum weight capacity.
--   This solves the 0/1 Knapsack problem using dynamic programming.
--
--   Arguments:
--     n     - The maximum weight capacity of the knapsack.
--     items - A list of (weight, value) pairs for each item.
--
--   Returns:
--     The maximum total value of items that can be placed in the knapsack.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = dpTable ! (numItems, capacity)
  where
    numItems = length items

    -- Convert the list of items to an array for efficient O(1) access
    itemArray :: Array Int (Int, Int)
    itemArray = array (0, numItems - 1) (zip [0..] items)

    -- Define the bounds for the DP table: (item_index, current_capacity)
    -- item_index ranges from 0 (no items) to numItems (all items)
    -- current_capacity ranges from 0 to 'capacity'
    bounds = ((0, 0), (numItems, capacity))

    -- Create the DP table using array construction.
    -- dpTable ! (i, w) stores the maximum value using the first 'i' items
    -- with a knapsack capacity of 'w'.
    dpTable :: Array (Int, Int) Int
    dpTable = array bounds [ ((i, w), calculateValue i w)
                           | i <- [0..numItems]
                           , w <- [0..capacity]
                           ]

    -- Helper function to calculate the value for a specific (i, w) cell in the DP table.
    calculateValue :: Int -> Int -> Int
    calculateValue i w
      -- Base case: If no items are considered or no capacity is available, the value is 0.
      | i == 0 || w == 0 = 0
      -- If the current item's weight is greater than the current capacity,
      -- we cannot include this item. So, the value is the same as without this item.
      | itemWeight > w   = dpTable ! (i-1, w)
      -- Otherwise, we have two choices:
      -- 1. Don't take the current item: value is dpTable ! (i-1, w)
      -- 2. Take the current item: value is itemValue + dpTable ! (i-1, w - itemWeight)
      -- We choose the maximum of these two options.
      | otherwise        = max (dpTable ! (i-1, w))
                               (itemValue + dpTable ! (i-1, w - itemWeight))
      where
        -- Get the current item's weight and value from the itemArray.
        -- Note: itemArray is 0-indexed, so we use (i-1) to get the i-th item.
        (itemWeight, itemValue) = itemArray ! (i-1)

