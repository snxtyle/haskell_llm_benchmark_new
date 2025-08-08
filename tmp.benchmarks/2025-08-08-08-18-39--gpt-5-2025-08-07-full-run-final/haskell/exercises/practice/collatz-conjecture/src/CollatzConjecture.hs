module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1     = Nothing
  | otherwise = Just (steps 0 n)
  where
    steps :: Integer -> Integer -> Integer
    steps acc 1 = acc
    steps acc k
      | even k    = steps (acc + 1) (k `div` 2)
      | otherwise = steps (acc + 1) (3 * k + 1)
