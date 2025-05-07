module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo nMax
  | nMax < 2 = []
  | otherwise  = sieveInternal [2..nMax]
  where
    -- sieveInternal performs the Sieve of Eratosthenes algorithm.
    -- It takes a list of candidate numbers and recursively filters out multiples.
    -- nMax (the upper limit for primes) is captured from the scope of primesUpTo.
    sieveInternal :: [Integer] -> [Integer]
    sieveInternal [] = []
    sieveInternal (p:xs)
      -- Optimization: If p*p > nMax, all remaining numbers in xs must be prime.
      -- Any composite number x <= nMax has a prime factor <= sqrt(nMax).
      -- If p > sqrt(nMax), then p is greater than any potential smallest prime factor
      -- of remaining composites. Such composites would have been filtered out by
      -- primes smaller than p. Thus, remaining numbers in xs are prime.
      | p*p > nMax = p : xs
      | otherwise  = p : sieveInternal (removeMultiplesOfPFrom xs p)

    -- removeMultiplesOfPFrom filters out multiples of a given prime 'primeFactor'
    -- from a list of 'candidates'.
    -- It starts generating multiples from primeFactor*primeFactor, as smaller multiples
    -- (k*primeFactor where k < primeFactor) would have been removed by earlier, smaller primes.
    removeMultiplesOfPFrom :: [Integer] -> Integer -> [Integer]
    removeMultiplesOfPFrom candidates primeFactor =
      let multiplesToRemove = takeWhile (<= nMax) (iterate (+primeFactor) (primeFactor*primeFactor))
      in filterListDiff candidates multiplesToRemove

    -- filterListDiff efficiently computes the difference of two sorted lists: source - toBeRemoved.
    -- It returns elements from 'source' that are not in 'toBeRemoved'.
    -- Both lists must be sorted.
    -- This is an O(N+M) operation, where N and M are the lengths of the lists.
    filterListDiff :: [Integer] -> [Integer] -> [Integer]
    filterListDiff source [] = source -- Nothing left to remove, return remaining source
    filterListDiff [] _      = []     -- Source is empty, result is empty
    filterListDiff (s:ss) (r:rs)
      | s < r  = s : filterListDiff ss (r:rs) -- s is smaller than r; s is not in toBeRemoved (yet). Keep s.
      | s == r = filterListDiff ss rs         -- s equals r; s is in toBeRemoved. Discard s. Advance both.
      | s > r  = filterListDiff (s:ss) rs     -- r is smaller than s; r is not in source at current s. Discard r.
