module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x =
    let (low, high) = bounds array
        -- Helper function for binary search
        binarySearch :: Int -> Int -> Maybe Int
        binarySearch l r
            | l > r = Nothing -- Search space is empty
            | otherwise =
                let mid = l + (r - l) `div` 2 -- Calculate middle index
                    val = array ! mid           -- Get value at middle index
                in if val == x
                   then Just mid               -- Found the element
                   else if val < x
                        then binarySearch (mid + 1) r -- Search in the right half
                        else binarySearch l (mid - 1) -- Search in the left half
    in binarySearch low high -- Start the search with the full array bounds
