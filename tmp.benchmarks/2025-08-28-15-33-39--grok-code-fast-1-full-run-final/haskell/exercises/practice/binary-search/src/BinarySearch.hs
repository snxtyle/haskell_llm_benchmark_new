module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x =
  let (low, high) = bounds array
  in search low high
  where
    search l h
      | l > h = Nothing
      | otherwise =
          let mid = l + (h - l) `div` 2
              val = array ! mid
          in case compare val x of
               EQ -> Just mid
               LT -> search (mid + 1) h
               GT -> search l (mid - 1)
