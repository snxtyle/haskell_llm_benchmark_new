module Knapsack (maximumValue) where

import Data.Array (Array, (!), (//), listArray)
import Data.List  (foldl')

-- | Compute the maximum achievable value for a knapsack with the given
--   capacity when each item can be taken at most once.
--
--   The first argument is the knapsack capacity (a non-negative weight limit).
--   The second argument is a list of items, each represented as a pair
--   (weight, value).  All weights and values must be positive.
--
--   The implementation uses the classic dynamic-programming solution
--   with a one-dimensional table updated from right to left for each item,
--   yielding a time complexity of O(capacity * number_of_items) and
--   a space complexity of O(capacity).
maximumValue :: Int          -- ^ Knapsack capacity
             -> [(Int, Int)] -- ^ List of (weight, value) pairs
             -> Int          -- ^ Maximum attainable value
maximumValue capacity items
  | capacity <= 0 = 0
  | null items    = 0
  | otherwise     = dp ! capacity
  where
    -- Discard items that are too heavy or have non-positive weight/value.
    usableItems = [ (w, v) | (w, v) <- items, w > 0, v > 0, w <= capacity ]

    -- Initial DP table: all zeroes.
    initial :: Array Int Int
    initial = listArray (0, capacity) (repeat 0)

    -- Update the DP table for a single item (w, v).
    step :: Array Int Int -> (Int, Int) -> Array Int Int
    step arr (w, v) = arr // updates
      where
        updates = [ (j, max (arr ! j) (arr ! (j - w) + v))
                  | j <- [capacity, capacity - 1 .. w] ]

    -- Final DP table after processing all items.
    dp :: Array Int Int
    dp = foldl' step initial usableItems
