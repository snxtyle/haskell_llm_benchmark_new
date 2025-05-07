module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x =
  let (lowBound, highBound) = bounds array
      
      -- Helper function to perform the recursive binary search
      search :: Int -> Int -> Maybe Int
      search low high
        | low > high = Nothing -- Base case: search space is exhausted, element not found
        | otherwise =
            let mid = low + (high - low) `div` 2 -- Calculate middle index to avoid overflow and ensure it's within bounds
                midVal = array ! mid             -- Get the value at the middle index
            in case compare midVal x of
                 EQ -> Just mid         -- Element found at mid index
                 LT -> search (mid + 1) high -- Target is greater, search in the right half
                 GT -> search low (mid - 1)   -- Target is smaller, search in the left half

  in if lowBound > highBound -- Check if the array is empty (e.g., bounds (1,0))
     then Nothing
     else search lowBound highBound -- Start the search with the array's actual bounds
