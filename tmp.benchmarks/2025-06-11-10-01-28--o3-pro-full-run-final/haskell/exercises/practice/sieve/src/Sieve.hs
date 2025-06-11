module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

-- | Compute all prime numbers less than or equal to the given bound
--   using the Sieve of Eratosthenes while avoiding any division or
--   remainder operations.
primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = 2 : sieve [3,5..n]   -- start with 2, then only odds
  where
    -- The core sieve procedure that repeatedly removes the multiples
    -- of each discovered prime.
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (p:xs)
      | p * p > n = p : xs           -- no more composites past √n
      | otherwise = p : sieve (diff xs multiples)
      where
        -- All multiples of the current prime starting at p*p.
        -- Step is 2*p because p is odd and even multiples are skipped.
        multiples = takeWhile (<= n) [p * p, p * p + 2 * p ..]

    -- Difference of two sorted (ascending) lists.
    -- Keeps elements of the first list that are not in the second.
    diff :: [Integer] -> [Integer] -> [Integer]
    diff xs [] = xs
    diff [] _  = []
    diff a@(x:xs) b@(y:ys)
      | x < y   = x : diff xs b
      | x == y  = diff xs ys
      | x > y   = diff a ys
