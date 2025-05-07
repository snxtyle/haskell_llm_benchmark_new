module Knapsack (maximumValue) where

import Control.Monad       (forM_, when)
import Control.Monad.ST    (ST, runST)
import Data.Array.ST       (STArray, newArray, readArray, writeArray)

-- | Compute the maximum total value that fits into a knapsack of the given
--   capacity.  Each item is a pair (weight, value).  All weights and values
--   are assumed to be non‑negative; items with weight greater than the
--   capacity are ignored.
maximumValue :: Int            -- ^ Knapsack capacity
             -> [(Int, Int)]   -- ^ List of items (weight, value)
             -> Int            -- ^ Maximum achievable value
maximumValue capacity items
  | capacity <= 0 = 0
  | null items    = 0
  | otherwise     = runST (knapsackST capacity items)

-- Internal: mutable‑array implementation in the ST monad
knapsackST :: Int -> [(Int, Int)] -> ST s Int
knapsackST cap its = do
  dp <- newArray (0, cap) 0 :: ST s (STArray s Int Int)

  -- Process each item
  forM_ its $ \(wt, val) ->
    when (wt <= cap) $
      -- Traverse weights in descending order to ensure 0‑1 behaviour
      forM_ [cap, cap-1 .. wt] $ \w -> do
        prev <- readArray dp (w - wt)
        cur  <- readArray dp w
        let candidate = prev + val
        when (candidate > cur) $
          writeArray dp w candidate

  -- Result is the best value achievable with total weight ≤ cap
  readArray dp cap
