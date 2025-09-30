module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (countSteps n 0)
  where
    countSteps :: Integer -> Integer -> Integer
    countSteps 1 steps = steps
    countSteps x steps
      | even x    = countSteps (x `div` 2) (steps + 1)
      | otherwise = countSteps (3 * x + 1) (steps + 1)
