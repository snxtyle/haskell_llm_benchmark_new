module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing  -- Input must be a positive integer
  | otherwise = Just (steps n 0)
  where
    steps 1 count = count  -- Reached 1, return the step count
    steps x count
      | even x    = steps (x `div` 2) (count + 1)  -- Even: divide by 2
      | otherwise = steps (3 * x + 1) (count + 1)  -- Odd: 3x + 1
