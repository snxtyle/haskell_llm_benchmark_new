module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = 
  let (low, high) = bounds array
      search l h
        | l > h     = Nothing
        | otherwise = 
            let mid = l + (h - l) `div` 2
                midVal = array ! mid
            in case compare x midVal of
                 EQ -> Just mid
                 LT -> search l (mid - 1)
                 GT -> search (mid + 1) h
  in search low high
