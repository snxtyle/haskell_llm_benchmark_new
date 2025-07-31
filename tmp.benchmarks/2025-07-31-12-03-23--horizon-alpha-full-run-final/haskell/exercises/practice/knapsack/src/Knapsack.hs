module Knapsack (maximumValue) where

-- | Compute the maximum total value for the 0/1 knapsack problem.
--   The first argument is the maximum weight capacity.
--   The second argument is a list of (weight, value) pairs for items.
--   Each item can be taken at most once.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items
  | capacity <= 0 = 0
  | null items   = 0
  | otherwise    = last dp
  where
    -- dp[w] will store the best value achievable with capacity w
    -- We build it by folding each item, updating from high to low capacities
    initial :: [Int]
    initial = replicate (capacity + 1) 0

    step :: [Int] -> (Int, Int) -> [Int]
    step acc (wt, val)
      | wt <= 0 = acc  -- ignore non-positive weights defensively
      | otherwise =
          -- Update from high to low to ensure each item is only used once
          let update w
                | w < wt    = acc !! w
                | otherwise = max (acc !! w) (acc !! (w - wt) + val)
          in [ update w | w <- [0..capacity] ]

    dp :: [Int]
    dp = foldl step initial items
