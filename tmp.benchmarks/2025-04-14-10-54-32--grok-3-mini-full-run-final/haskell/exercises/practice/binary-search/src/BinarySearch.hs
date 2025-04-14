module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = go (bounds array)
  where
    go (low, high)
      | low > high = Nothing  -- The search range is empty, item not found
      | otherwise =
          let mid = low + ((high - low) `div` 2)  -- Calculate middle index
              val = array ! mid  -- Get the value at mid index
          in if val == x
             then Just mid  -- Found the item
             else if val < x
                  then go (mid + 1, high)  -- Search the right half
                  else go (low, mid - 1)  -- Search the left half
