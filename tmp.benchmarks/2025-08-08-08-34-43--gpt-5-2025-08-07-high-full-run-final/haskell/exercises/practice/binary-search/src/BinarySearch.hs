module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x = go lo hi
  where
    (lo, hi) = bounds arr

    go l h
      | l > h = Nothing
      | otherwise =
          let mid = l + (h - l) `div` 2
              midVal = arr ! mid
          in case compare x midVal of
               LT -> go l (mid - 1)
               GT -> go (mid + 1) h
               EQ -> Just mid
