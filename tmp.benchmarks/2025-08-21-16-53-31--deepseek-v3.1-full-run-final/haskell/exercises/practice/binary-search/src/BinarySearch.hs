module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array) array x

binarySearch :: Ord a => (Int, Int) -> Array Int a -> a -> Maybe Int
binarySearch (low, high) array x
    | low > high = Nothing
    | otherwise =
        let mid = low + (high - low) `div` 2
            midVal = array ! mid
        in case compare x midVal of
            EQ -> Just mid
            LT -> binarySearch (low, mid - 1) array x
            GT -> binarySearch (mid + 1, high) array x
