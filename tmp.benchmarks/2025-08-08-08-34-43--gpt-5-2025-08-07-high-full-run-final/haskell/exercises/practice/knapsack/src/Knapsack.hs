module Knapsack (maximumValue) where

import Data.List (foldl')

-- | Compute the maximum achievable value for a knapsack with given capacity.
--   Items are given as (weight, value). Each item can be taken at most once.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items
  | capacity <= 0 = 0
  | otherwise     = last finalDP
  where
    initial :: [Int]
    initial = replicate (capacity + 1) 0

    finalDP :: [Int]
    finalDP = foldl' step initial items

    step :: [Int] -> (Int, Int) -> [Int]
    step dp (w, v)
      | w <= 0       = zipWith max dp (map (+ v) dp)            -- zero or negative weight acts like free item
      | w > capacity = dp                                       -- too heavy, cannot include
      | otherwise    = zipWith max dp (replicate w 0 ++ map (+ v) dp)
