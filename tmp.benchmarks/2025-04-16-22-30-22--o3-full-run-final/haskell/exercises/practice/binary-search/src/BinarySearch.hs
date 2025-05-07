module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

-- | Perform a binary search on a sorted array.
--
--   The function expects the array to be sorted in ascending order.
--   It returns @Just index@ if the sought element is present and
--   @Nothing@ otherwise.
--
--   The implementation works for arrays with arbitrary (inclusive)
--   lower and upper bounds.
find :: Ord a => Array Int a -> a -> Maybe Int
find arr target = go lo hi
  where
    (lo, hi) = bounds arr

    go :: Int -> Int -> Maybe Int
    go low high
      | low > high = Nothing
      | otherwise  =
          let mid = low + (high - low) `div` 2
              val = arr ! mid
           in case compare target val of
                LT -> go low (mid - 1)
                EQ -> Just mid
                GT -> go (mid + 1) high
