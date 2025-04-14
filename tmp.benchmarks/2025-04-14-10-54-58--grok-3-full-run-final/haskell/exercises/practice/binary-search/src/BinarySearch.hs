module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch x (bounds array)
  where
    binarySearch x (low, high)
      | low > high = Nothing
      | otherwise =
          let mid = low + (high - low) `div` 2
              midVal = array ! mid
          in case compare midVal x of
               EQ -> Just mid
               LT -> binarySearch x (mid + 1, high)
               GT -> binarySearch x (low, mid - 1)
