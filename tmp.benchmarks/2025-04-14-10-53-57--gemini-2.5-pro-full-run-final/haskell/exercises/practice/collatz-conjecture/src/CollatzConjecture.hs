module CollatzConjecture (collatz) where

-- | Calculates the number of steps required to reach 1
-- | using the Collatz Conjecture rules.
-- |
-- | Takes a positive integer n.
-- | If n is even, divide n by 2 to get n / 2.
-- | If n is odd, multiply n by 3 and add 1 to get 3n + 1.
-- | Repeat the process until n reaches 1.
-- |
-- | Returns Nothing if the input n is not positive.
-- | Otherwise, returns Just the number of steps.
collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing -- Input must be positive
  | otherwise = Just (calculateSteps n 0)
  where
    -- Helper function to recursively calculate steps
    calculateSteps :: Integer -> Integer -> Integer
    calculateSteps current steps
      | current == 1 = steps -- Base case: reached 1
      | even current = calculateSteps (current `div` 2) (steps + 1) -- Even step
      | odd current  = calculateSteps (3 * current + 1) (steps + 1) -- Odd step
