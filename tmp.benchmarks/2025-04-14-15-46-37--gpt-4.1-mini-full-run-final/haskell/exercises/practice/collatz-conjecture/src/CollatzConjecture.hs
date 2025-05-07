module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (steps n)
  where
    steps 1 = 0
    steps x
      | even x    = 1 + steps (x `div` 2)
      | otherwise = 1 + steps (3 * x + 1)
