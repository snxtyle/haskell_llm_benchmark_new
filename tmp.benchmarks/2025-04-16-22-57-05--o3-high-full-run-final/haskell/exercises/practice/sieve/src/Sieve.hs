module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.List ((\\))

-- | Produce all prime numbers ≤ n using the Sieve of Eratosthenes,
--   without employing any division/remainder operations.
primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2 .. n]
  where
    sieve []     = []
    sieve (p:xs)
      -- When p² exceeds the upper limit, the remaining numbers are prime.
      | p * p > n = p : xs
      | otherwise = p : sieve (xs \\ multiples p)

    -- Multiples of the current prime starting from p².
    multiples p = [p * p, p * p + p .. n]
