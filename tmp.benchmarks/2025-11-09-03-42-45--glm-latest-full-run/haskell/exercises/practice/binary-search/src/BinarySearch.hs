module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go low high
  where
    (low, high) = bounds array

    go :: Int -> Int -> Maybe Int
    go lo hi
      | lo > hi         = Nothing  -- Base case: not found
      | x == midVal     = Just mid -- Base case: found
      | x < midVal      = go lo (mid - 1) -- Search left half
      | otherwise       = go (mid + 1) hi -- Search right half
      where
        mid = lo + (hi - lo) `div` 2
        midVal = array ! mid
