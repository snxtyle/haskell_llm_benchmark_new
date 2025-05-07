module CollatzConjecture (collatz) where

-- | Calculate the number of steps required to reach 1
--   following the Collatz process.
--
--   For a positive input 'n', the function returns @Just steps@,
--   where @steps@ is the total number of transformations needed
--   to reach the value 1.
--
--   If 'n' is not a strictly positive integer (i.e. 0 or negative),
--   the function returns 'Nothing'.
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
  | otherwise = Just (go 0 n)
  where
    go :: Integer -> Integer -> Integer
    go steps 1 = steps
    go steps x
      | even x    = go (steps + 1) (x `div` 2)
      | otherwise = go (steps + 1) (3 * x + 1)
