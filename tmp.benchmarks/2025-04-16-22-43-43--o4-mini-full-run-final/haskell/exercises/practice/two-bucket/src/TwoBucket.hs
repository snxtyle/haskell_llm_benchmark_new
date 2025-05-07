module TwoBucket (measure) where

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (c1, c2) target
  | target > max c1 c2         = Nothing
  | target `mod` gcd c1 c2 /= 0 = Nothing
  | otherwise                   = Just $ simulate c1 c2 target

-- simulate pouring from bucket1 (capacity fromCap) into bucket2 (capacity toCap)
-- Returns (numberOfActions, (finalBucket1Amount, finalBucket2Amount))
simulate :: Int -> Int -> Int -> (Int, (Int, Int))
simulate fromCap toCap target = go 0 0 0
  where
    go from to steps
      -- stop when either bucket reaches the target
      | from == target || to == target = (steps, (from, to))
      -- if bucket1 is empty, fill it
      | from == 0                      = go fromCap to     (steps + 1)
      -- if bucket2 is full, empty it
      | to   == toCap                  = go from     0     (steps + 1)
      -- otherwise pour from bucket1 into bucket2
      | otherwise                      =
          let pourAmt = min from (toCap - to)
          in go (from - pourAmt) (to + pourAmt) (steps + 1)
