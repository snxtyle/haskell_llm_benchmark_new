module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set
import Data.Function ((&))

-- | Calculates the sum of all unique multiples of a given set of factors, below a given limit.
--
-- For example, for factors [3, 5] and limit 20, the multiples are:
-- - 3: [3, 6, 9, 12, 15, 18]
-- - 5: [5, 10, 15]
-- The unique multiples are [3, 5, 6, 9, 10, 12, 15, 18], and their sum is 78.
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  factors
    & filter (/= 0) -- Remove zero factors to avoid infinite lists
    & map (\f -> takeWhile (< limit) [f, 2*f..]) -- Generate multiples for each factor
    & concat -- Combine all lists of multiples
    & Set.fromList -- Convert to a set to get unique values
    & Set.toList -- Convert back to a list
    & sum -- Sum the unique multiples
