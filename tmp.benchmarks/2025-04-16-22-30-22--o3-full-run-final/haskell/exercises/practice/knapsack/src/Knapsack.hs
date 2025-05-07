module Knapsack (maximumValue) where

import Data.Array (Array, (!), (//), elems, listArray)
import Data.List  (foldl')

-- | Compute the maximum total value that fits into a knapsack
--   with the given weight capacity.
--
--   The first argument is the capacity of the knapsack.
--   The second argument is a list of items, each represented as
--   a pair (weight, value).
--
--   This function implements the classic 0/1‑knapsack dynamic
--   programming algorithm with a time complexity of
--   O(capacity * number_of_items) and memory usage O(capacity).
maximumValue :: Int          -- ^ Knapsack capacity
             -> [(Int, Int)] -- ^ List of (weight, value) items
             -> Int          -- ^ Maximum achievable value
maximumValue capacity items
  | capacity <= 0 = 0
  | null items    = 0
  | otherwise     = maximum (elems finalArr)
  where
    -- Initial DP array: for every weight 0..capacity store the best value.
    initialArr :: Array Int Int
    initialArr = listArray (0, capacity) (repeat 0)

    -- Fold over all items, updating the DP array.
    finalArr :: Array Int Int
    finalArr = foldl' update initialArr items

    -- Update the DP array with a single item (wt, val).
    -- We iterate weights in descending order to avoid using
    -- the same item multiple times.
    update :: Array Int Int -> (Int, Int) -> Array Int Int
    update arr (wt, val)
      | wt <= 0        = arr             -- Ignore non‑positive weights
      | wt > capacity  = arr             -- Item too heavy, skip
      | otherwise      = arr // changes
      where
        changes = [ (w, best w) | w <- [capacity, capacity - 1 .. wt] ]
        best w  = max (arr ! w) (arr ! (w - wt) + val)
