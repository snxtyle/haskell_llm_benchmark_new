module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

import Data.List ((\\))

-- | Calculate all prime numbers less than or equal to the supplied upper bound
--   using the classic Sieve of Eratosthenes algorithm.
--
--   The implementation purposefully avoids every division‑related operator
--   (`div`, `mod`, `rem`, ...) by generating the multiples of each discovered
--   prime through repeated addition and then removing those multiples from the
--   working list.
--
--   Example:
--
--   >>> primesUpTo 10
--   [2,3,5,7]
primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2 .. n]
  where
    sieve :: [Integer] -> [Integer]
    sieve []       = []
    sieve (p : xs) = p : sieve (xs \\ multiples)
      where
        -- All multiples of the current prime starting from p*p, generated
        -- without using multiplication inside the loop (iterate (+p)).
        multiples :: [Integer]
        multiples
          | pSquared > n = []
          | otherwise    = takeWhile (<= n) $ iterate (+ p) pSquared
          where
            pSquared = p * p
