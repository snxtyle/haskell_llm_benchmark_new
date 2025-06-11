module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = search (bounds array)
  where
    search (low, high)
      | low > high = Nothing
      | otherwise =
          let mid = low + (high - low) `div` 2
              midVal = array ! mid
          in case compare x midVal of
               EQ -> Just mid
               LT -> search (low, mid - 1)
               GT -> search (mid + 1, high)
