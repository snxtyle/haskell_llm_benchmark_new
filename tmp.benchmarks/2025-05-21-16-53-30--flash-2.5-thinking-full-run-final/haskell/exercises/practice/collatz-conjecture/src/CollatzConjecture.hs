module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing -- Invalid input: n must be a positive integer
  | n == 1    = Just 0  -- Base case: 1 requires 0 steps
  | even n    = fmap (+1) (collatz (n `div` 2)) -- If even, divide by 2 and add 1 to steps
  | otherwise = fmap (+1) (collatz (3 * n + 1)) -- If odd, multiply by 3 and add 1, then add 1 to steps
