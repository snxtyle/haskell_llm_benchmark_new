module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = map fst $ filter snd $ zip [2..] finalSieve
  where
    limit = n
    -- The highest prime we need to sieve with is sqrt(n).
    -- Any composite number c <= n must have a prime factor <= sqrt(c) <= sqrt(n).
    sieveBound = floor . sqrt $ fromIntegral limit
    -- The boolean list, where index i corresponds to number i+2.
    -- True means "potentially prime", False means "composite".
    initialSieve = replicate (fromIntegral (limit - 1)) True

    -- Recursive function to perform the sieving.
    -- It takes the current number to check, the upper bound for sieving,
    -- and the current state of the sieve.
    performSieve :: Integer -> Integer -> [Bool] -> [Bool]
    performSieve p bound sieve
      | p > bound = sieve
      | otherwise =
          let pIndex = fromIntegral (p - 2)
              -- Check if p is still marked as prime in the sieve.
              isPPrime = sieve !! pIndex
          in if isPPrime
             then performSieve (p + 1) bound (markMultiples p sieve)
             else performSieve (p + 1) bound sieve

    -- Function to mark all multiples of a prime p as False.
    -- It starts marking from p*p, as smaller multiples would have been
    -- marked by smaller primes.
    markMultiples :: Integer -> [Bool] -> [Bool]
    markMultiples p sieve =
      let startIdx = fromIntegral (p * p - 2)
          step = fromIntegral p
          size = length sieve
          -- Generate the list of indices to mark.
          indicesToMark = [startIdx, startIdx + step .. size - 1]
      in markIndices indicesToMark sieve

    -- Helper function to mark a list of indices in a boolean list.
    markIndices :: [Int] -> [Bool] -> [Bool]
    markIndices [] s = s
    markIndices (i:is) s = markIndices is (markIndex i s)

    -- Helper function to mark a single index as False.
    markIndex :: Int -> [Bool] -> [Bool]
    markIndex i s = take i s ++ [False] ++ drop (i + 1) s

    -- Run the sieve algorithm on the initial list.
    finalSieve = performSieve 2 sieveBound initialSieve
