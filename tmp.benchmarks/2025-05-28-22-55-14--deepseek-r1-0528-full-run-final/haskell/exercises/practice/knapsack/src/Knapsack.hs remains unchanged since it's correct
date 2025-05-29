module Knapsack (maximumValue) where

import Data.Array (listArray, (!), elems)
import Data.List (foldl')

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue n items = maximum (elems dp_final)
  where
    dp0 = listArray (0, n) (repeat 0)
    dp_final = foldl' update dp0 items
    update dp (w, v) = listArray (0, n) [ if j >= w 
                                           then max (dp!j) (dp!(j-w) + v) 
                                           else dp!j 
                                         | j <- [0..n] ]
