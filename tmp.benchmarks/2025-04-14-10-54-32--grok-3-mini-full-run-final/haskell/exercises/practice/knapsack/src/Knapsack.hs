module Knapsack (maximumValue) where

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items
  | capacity < 0 = 0  -- Though not expected, handle negative capacity as 0
  | otherwise =
      let initialDp = replicate (capacity + 1) 0  -- [0, 0, ..., 0] for weights 0 to capacity
          finalDp = foldl updateRow initialDp items  -- Fold over items to update DP array
          updateRow dp (weight, value) =  -- Function to update a single row
            [ if i >= weight
                then max (dp !! i) (value + (dp !! (i - weight)))  -- Take max: include or exclude item
                else dp !! i  -- Can't include item
            | i <- [0..capacity]
            ]
      in finalDp !! capacity  -- The value at index 'capacity' is the result
