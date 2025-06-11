module Knapsack (maximumValue) where

import Data.List (foldl')

-- | Solves the 0/1 knapsack problem.
-- Given a list of items, each with a weight and a value, and a maximum
-- knapsack capacity, determines the maximum total value of items that can
-- be carried in the knapsack.
maximumValue
  :: Int            -- ^ Maximum knapsack weight capacity
  -> [(Int, Int)]   -- ^ List of items as (weight, value) tuples
  -> Int            -- ^ Maximum possible value
maximumValue capacity items
  | capacity <= 0 = 0
  | otherwise =
    let
      -- This is a dynamic programming solution to the 0/1 knapsack problem.
      -- We use a list `dp` of size `capacity + 1`, where `dp !! w` stores the
      -- maximum value achievable with a knapsack of capacity `w`.
      -- We iterate through each item and update the `dp` list.

      -- Initialize the DP list with all zeros.
      initialDP = replicate (capacity + 1) 0

      -- `updateDP` is a helper function for the fold. It takes the DP list
      -- calculated for the preceding items and the current item, and returns
      -- the new DP list that accounts for the current item.
      updateDP :: [Int] -> (Int, Int) -> [Int]
      updateDP prevDP (itemWeight, itemValue)
        -- Ignore items with non-positive weight or weight greater than capacity.
        | itemWeight <= 0 || itemWeight > capacity = prevDP
        | otherwise =
            let
              -- For each capacity `w` from `itemWeight` to `capacity`, we choose the better of two options:
              -- 1. Don't include the current item: The value is `prevDP !! w`.
              -- 2. Include the current item: The value is `itemValue + prevDP !! (w - itemWeight)`.

              -- To implement this efficiently with lists, we split `prevDP`.
              -- `unchanged`: for capacities less than `itemWeight`, the value cannot change.
              -- `toBeUpdated`: for capacities from `itemWeight` up to `capacity`.
              (unchanged, toBeUpdated) = splitAt itemWeight prevDP

              -- `relevantOldValues` are the values from `prevDP` for capacities `0` to `capacity - itemWeight`.
              -- These are needed for option 2 above.
              relevantOldValues = prevDP

              -- `zipWith` creates the updated part of the DP list.
              updated = zipWith (\valWithoutItem valForSmallerCapacity ->
                                   max valWithoutItem (itemValue + valForSmallerCapacity))
                                toBeUpdated relevantOldValues
            in
              unchanged ++ updated

      -- Use a strict left fold to build the final DP list by iterating through all items.
      finalDP = foldl' updateDP initialDP items
    in
      -- The answer is the last element of the final DP list, corresponding to the maximum capacity.
      last finalDP
