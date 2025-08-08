module BinarySearch (find) where

import Data.Array (Array, bounds, (!))

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go lo hi
  where
    (lo, hi) = bounds array

    go l h
      | l > h = Nothing
      | otherwise =
          let mid = l + (h - l) `div` 2
              v = array ! mid
          in case compare x v of
               LT -> go l (mid - 1)
               GT -> go (mid + 1) h
               EQ -> Just mid
