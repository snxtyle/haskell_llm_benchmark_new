module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = 
    let (low, high) = bounds array
        search l h
            | l > h     = Nothing
            | otherwise = 
                let mid = (l + h) `div` 2
                in case compare (array ! mid) x of
                     EQ -> Just mid
                     LT -> search (mid + 1) h
                     GT -> search l (mid - 1)
    in search low high
