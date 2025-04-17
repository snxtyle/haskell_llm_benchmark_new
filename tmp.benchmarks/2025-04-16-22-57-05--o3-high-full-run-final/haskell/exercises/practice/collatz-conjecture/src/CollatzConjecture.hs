module CollatzConjecture (collatz) where

-- | Calculate the number of steps required to reach 1 using the Collatz
--   process. For non‑positive inputs the function returns 'Nothing',
--   otherwise it returns 'Just steps', where @steps@ is the count of
--   iterations needed to transform the starting value into 1.
--
--   Examples:
--
--   >>> collatz 12
--   Just 9
--
--   >>> collatz 1
--   Just 0
--
--   >>> collatz 0
--   Nothing
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
