module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (collatz' n 0)
  where
    collatz' :: Integer -> Integer -> Integer
    collatz' 1 steps = steps
    collatz' current steps
      | even current = collatz' (current `div` 2) (steps + 1)
      | otherwise    = collatz' (3 * current + 1) (steps + 1)
