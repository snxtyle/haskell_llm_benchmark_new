module Knapsack (maximumValue) where

-- | Compute the maximum total value of items that fit into a knapsack
--   of capacity 'cap'. Each item is given as a (weight, value) pair.
maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue cap items = dp !! cap
  where
    -- dp[i] will hold the max value for capacity i
    initDp = replicate (cap + 1) 0

    -- For each item (w, v), update dp from 0..cap:
    -- if i < w, can't take the item => keep dp_i
    -- otherwise, choose max of not taking or taking (v + dp[i - w])
    step dpArray (w, v) =
      [ if i < w
          then dp_i
          else max dp_i (v + dpArray !! (i - w))
      | (i, dp_i) <- zip [0 .. cap] dpArray
      ]

    dp = foldl step initDp items
