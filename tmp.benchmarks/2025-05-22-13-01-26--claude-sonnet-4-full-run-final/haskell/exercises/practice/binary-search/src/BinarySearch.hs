module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array) array x
  where
    binarySearch (low, high) arr target
      | low > high = Nothing
      | target == midValue = Just mid
      | target < midValue = binarySearch (low, mid - 1) arr target
      | otherwise = binarySearch (mid + 1, high) arr target
      where
        mid = low + (high - low) `div` 2
        midValue = arr ! mid
