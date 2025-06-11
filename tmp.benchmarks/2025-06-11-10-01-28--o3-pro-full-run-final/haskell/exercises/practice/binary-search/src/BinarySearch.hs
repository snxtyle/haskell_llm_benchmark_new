module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

-- | Perform a binary search on a sorted array.
--
-- Given a sorted 'Array' of elements and a target element,
-- return 'Just' the index of the element if it exists, or
-- 'Nothing' otherwise.
--
-- The implementation works for arrays with arbitrary integer
-- bounds (the first index need not be 0).
find :: Ord a => Array Int a -> a -> Maybe Int
find arr target = go lo hi
  where
    (lo, hi) = bounds arr

    go :: Int -> Int -> Maybe Int
    go l h
      | l > h     = Nothing
      | otherwise =
          let mid = l + (h - l) `div` 2
              midVal = arr ! mid
          in case compare target midVal of
               LT -> go l (mid - 1)
               GT -> go (mid + 1) h
               EQ -> Just mid
