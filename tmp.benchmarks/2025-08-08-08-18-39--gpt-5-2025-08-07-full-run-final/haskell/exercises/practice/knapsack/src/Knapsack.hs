module Knapsack (maximumValue) where

import Data.List (foldl')

-- maximumValue capacity items
-- items are given as (weight, value)
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items
  | capacity <= 0 = 0
  | otherwise     = last finalDP
  where
    n = capacity
    initialDP = replicate (n + 1) 0
    step dp (w, v) =
      -- For each capacity c, candidate is dp_old[c - w] + v if c >= w else 0
      -- Build that candidate list by shifting dp by w and adding v, then trim to n+1.
      let candidate = take (n + 1) (replicate w 0 ++ map (+ v) dp)
      in zipWith max dp candidate
    finalDP = foldl' step initialDP items
