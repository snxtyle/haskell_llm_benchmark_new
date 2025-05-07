module Knapsack (maximumValue) where

import Data.Array (Array, listArray, (!), (//))
import Data.List (foldl')

-- | Solves the 0/1 Knapsack problem.
-- Given a maximum capacity and a list of items with weights and values,
-- finds the maximum total value of items that can be carried without
-- exceeding the capacity.
--
-- Args:
--   maxW: The maximum weight capacity of the knapsack.
--   items: A list of tuples, where each tuple represents an item (weight, value).
--
-- Returns:
--   The maximum value that can be achieved.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue maxW items
  -- Handle edge case where maxW is non-positive.
  | maxW <= 0 = 0
  | otherwise = dp ! maxW
  where
    -- Create a DP array `dp` where `dp[w]` stores the maximum value achievable
    -- for a knapsack capacity of `w`.
    -- Initialize the array with 0s for all capacities from 0 to maxW.
    initialDp :: Array Int Int
    initialDp = listArray (0, maxW) (replicate (maxW + 1) 0)

    -- Iterate through each item and update the DP array based on that item.
    -- `foldl'` is used for strict accumulation.
    dp :: Array Int Int
    dp = foldl' updateDp initialDp items

    -- Function to update the DP array for a single item (itemW, itemV).
    updateDp :: Array Int Int -> (Int, Int) -> Array Int Int
    updateDp currentDp (itemW, itemV)
      -- Ensure item weight and value are positive as per typical knapsack constraints.
      -- If item weight is 0 but value is positive, it's like a free item,
      -- but the standard DP handles this. If weight is non-positive, skip.
      | itemW <= 0 = currentDp
      | otherwise =
          -- Iterate backwards through the capacities from maxW down to itemW.
          -- This backward iteration is crucial for the 0/1 knapsack property,
          -- ensuring that we don't use the same item multiple times within the
          -- calculation for a single capacity `w`.
          foldl' (\accDp w ->
                    -- Calculate the potential new maximum value if we include the current item.
                    let valueIncludingItem = itemV + accDp ! (w - itemW)
                    -- Update dp[w] only if including the item gives a better value.
                    in if valueIncludingItem > accDp ! w
                       then accDp // [(w, valueIncludingItem)]
                       else accDp
                 ) currentDp [maxW, maxW - 1 .. itemW]

