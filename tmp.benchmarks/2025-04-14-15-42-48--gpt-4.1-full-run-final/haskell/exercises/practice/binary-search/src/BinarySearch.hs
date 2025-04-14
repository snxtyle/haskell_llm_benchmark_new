module BinarySearch (find) where

import Data.Array

-- | Perform a binary search on a sorted array with arbitrary bounds.
-- Returns the index of the found element, or Nothing if not found.
find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go lo hi
  where
    (lo, hi) = bounds array
    go l h
      | l > h = Nothing
      | otherwise =
          let mid = l + (h - l) `div` 2
              midVal = array ! mid
          in case compare x midVal of
               LT -> go l (mid - 1)
               GT -> go (mid + 1) h
               EQ -> Just mid
