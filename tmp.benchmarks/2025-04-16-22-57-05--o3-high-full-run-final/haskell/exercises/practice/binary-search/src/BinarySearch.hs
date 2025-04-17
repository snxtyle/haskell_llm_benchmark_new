module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

-- | Find the index of an element in a sorted array using binary search.
--
--   The function returns 'Just i' if the element is present at index @i@,
--   or 'Nothing' when the element cannot be found.
--
--   Although the exercise signature fixes the index type to 'Int', the
--   implementation itself works for arrays whose lower bound is not
--   necessarily 0 because it always queries the actual bounds of the array
--   before starting the search.
find :: Ord a => Array Int a -> a -> Maybe Int
find arr target = go lo hi
  where
    (lo, hi) = bounds arr

    go :: Int -> Int -> Maybe Int
    go low high
      | low > high = Nothing
      | otherwise  =
          let mid    = low + (high - low) `div` 2
              midVal = arr ! mid
          in case compare midVal target of
               LT -> go (mid + 1) high
               GT -> go low (mid - 1)
               EQ -> Just mid
