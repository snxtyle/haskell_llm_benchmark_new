module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x
    | null range = Nothing
    | otherwise = search (bounds array)
  where
    range = indices array
    
    search (low, high)
        | low > high = Nothing
        | array ! mid == x = Just mid
        | array ! mid > x = search (low, mid - 1)
        | otherwise = search (mid + 1, high)
      where
        mid = low + (high - low) `div` 2
