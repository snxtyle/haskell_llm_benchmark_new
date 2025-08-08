module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array)
  where
    binarySearch (low, high)
      | low > high = Nothing
      | array ! mid > x = binarySearch (low, mid - 1)
      | array ! mid < x = binarySearch (mid + 1, high)
      | otherwise = Just mid
      where
        mid = low + (high - low) `div` 2
