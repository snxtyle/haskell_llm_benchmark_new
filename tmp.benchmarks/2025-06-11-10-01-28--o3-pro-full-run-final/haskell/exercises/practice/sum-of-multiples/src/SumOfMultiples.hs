module SumOfMultiples (sumOfMultiples) where

-- | Calculate the sum of all unique multiples of the given factors that are
--   less than the supplied upper limit.
--
--   Examples:
--   >>> sumOfMultiples [3, 5] 20
--   78
--
--   An empty list of factors yields 0, as there are no multiples to consider.
--   Factors equal to 0 are ignored because every number is a multiple of 0
--   (which would make the result ill-defined).
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let validFactors = filter (/= 0) factors
   in sum
        [ n
        | n <- [1 .. limit - 1]
        , any (\f -> n `mod` f == 0) validFactors
        ]
