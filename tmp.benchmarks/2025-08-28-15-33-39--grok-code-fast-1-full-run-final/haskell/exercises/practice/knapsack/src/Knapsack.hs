module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items = foldl update (replicate (capacity + 1) 0) items !! capacity
  where
    update dp (wgt, val) = [ if i < wgt then dp !! i else max (dp !! i) (dp !! (i - wgt) + val) | i <- [0..capacity] ]
