module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch array x lowerBound upperBound
  where
    (lowerBound, upperBound) = bounds array

binarySearch :: Ord a => Array Int a -> a -> Int -> Int -> Maybe Int
binarySearch array x low high
  | low > high = Nothing
  | otherwise =
      let mid = (low + high) `div` 2
          midValue = array ! mid
      in case compare x midValue of
           EQ -> Just mid
           LT -> binarySearch array x low (mid - 1)
           GT -> binarySearch array x (mid + 1) high
