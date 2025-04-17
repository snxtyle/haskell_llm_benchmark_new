module SumOfMultiples (sumOfMultiples) where

-- | Calculate the sum of all unique multiples of the given factors that are
--   strictly less than the supplied limit.
--
--   The function ignores factors that are zero, because every integer is a
--   multiple of zero and that would make the set of multiples unbounded.
--   Negative factors are treated by their absolute value, as only the divisor
--   magnitude matters for divisibility.
--
--   Examples:
--
--   >>> sumOfMultiples [3,5] 20
--   78
--
--   >>> sumOfMultiples [7,13,17] 20
--   51
--
--   >>> sumOfMultiples [] 10
--   0
--
--   >>> sumOfMultiples [0, 3] 10
--   18
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let -- Remove zeros to avoid division-by-zero and take absolute values to
      -- normalise negative factors.
      normalisedFactors = filter (/= 0) $ map abs factors
      -- Generate all numbers below the limit that are divisible by at least
      -- one factor.
      multiples        = [ n | n <- [1 .. limit - 1]
                             , any (\f -> n `mod` f == 0) normalisedFactors
                       ]
  in sum multiples
