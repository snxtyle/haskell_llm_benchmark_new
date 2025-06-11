module CollatzConjecture (collatz) where

-- | Compute the number of steps required to reach 1 using the Collatz process.
--   For n <= 0 the conjecture is undefined, so return Nothing.
--   For positive n, return Just steps, where steps is the count of iterations
--   needed to reach 1 (0 steps if n == 1).
collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (go n 0)
  where
    go :: Integer -> Integer -> Integer
    go 1 steps = steps
    go m steps
      | even m    = go (m `div` 2)     (steps + 1)
      | otherwise = go (3 * m + 1)     (steps + 1)
