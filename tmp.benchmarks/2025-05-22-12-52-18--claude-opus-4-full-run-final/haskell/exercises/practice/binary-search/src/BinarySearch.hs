module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array)
  where
    binarySearch (low, high)
      | low > high = Nothing
      | array ! mid == x = Just mid
      | array ! mid > x = binarySearch (low, mid - 1)
      | otherwise = binarySearch (mid + 1, high)
      where
        mid = low + (high - low) `div` 2
