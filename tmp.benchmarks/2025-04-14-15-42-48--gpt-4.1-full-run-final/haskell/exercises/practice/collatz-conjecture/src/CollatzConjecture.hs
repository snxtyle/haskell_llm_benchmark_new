module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (fromIntegral (collatzSteps n))
  where
    collatzSteps 1 = 0
    collatzSteps x
      | even x    = 1 + collatzSteps (x `div` 2)
      | otherwise = 1 + collatzSteps (3 * x + 1)
