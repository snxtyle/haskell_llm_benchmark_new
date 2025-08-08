module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | n == 1    = Just 0
  | otherwise = Just (collatzSteps n 0)
  where
    collatzSteps :: Integer -> Integer -> Integer
    collatzSteps 1 steps = steps
    collatzSteps num steps
      | even num  = collatzSteps (num `div` 2) (steps + 1)
      | otherwise = collatzSteps (3 * num + 1) (steps + 1)
