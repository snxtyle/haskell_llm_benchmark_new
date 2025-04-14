module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0    = Nothing
    | otherwise = Just (countSteps n 0)
  where
    countSteps 1 steps = steps
    countSteps num steps
        | even num  = countSteps (num `div` 2) (steps + 1)
        | otherwise = countSteps (3 * num + 1) (steps + 1)
