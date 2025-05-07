module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (steps n)
  where
    steps 1 = 0
    steps k
      | even k    = 1 + steps (k `div` 2)
      | otherwise = 1 + steps (3 * k + 1)
