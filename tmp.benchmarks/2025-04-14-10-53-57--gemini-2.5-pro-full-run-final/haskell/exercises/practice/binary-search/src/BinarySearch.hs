module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = search low high
  where
    (low, high) = bounds array -- Get the bounds of the array

    search :: Int -> Int -> Maybe Int
    search l h
      | l > h = Nothing -- Base case: element not found
      | otherwise =
          let mid = l + (h - l) `div` 2 -- Calculate middle index to avoid potential overflow
              midVal = array ! mid       -- Get the value at the middle index
          in case compare x midVal of
               EQ -> Just mid          -- Element found at mid
               LT -> search l (mid - 1) -- Element is in the lower half
               GT -> search (mid + 1) h -- Element is in the upper half
