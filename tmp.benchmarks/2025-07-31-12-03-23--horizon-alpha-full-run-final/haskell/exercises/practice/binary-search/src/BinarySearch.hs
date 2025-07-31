module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go lo hi
  where
    (lo, hi) = bounds array

    go l h
      | l > h = Nothing
      | otherwise =
          let mid = l + (h - l) `div` 2
              mv  = array ! mid
          in case compare mv x of
               EQ -> Just mid
               LT -> go (mid + 1) h
               GT -> go l (mid - 1)
