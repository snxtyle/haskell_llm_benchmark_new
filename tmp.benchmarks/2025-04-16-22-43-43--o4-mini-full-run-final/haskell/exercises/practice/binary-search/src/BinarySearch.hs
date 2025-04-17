module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go low high
  where
    (low, high) = bounds array

    go l h
      | l > h     = Nothing
      | otherwise =
          let mid = (l + h) `div` 2
              v   = array ! mid
          in case compare v x of
               EQ -> Just mid
               LT -> go (mid + 1) h
               GT -> go l (mid - 1)
