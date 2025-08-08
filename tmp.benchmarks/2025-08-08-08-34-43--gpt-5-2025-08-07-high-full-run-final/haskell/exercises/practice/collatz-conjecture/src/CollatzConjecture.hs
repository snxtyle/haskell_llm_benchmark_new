module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (go n 0)
  where
    go :: Integer -> Integer -> Integer
    go 1 steps = steps
    go k steps
      | even k    = go (k `div` 2) (steps + 1)
      | otherwise = go (3 * k + 1) (steps + 1)
