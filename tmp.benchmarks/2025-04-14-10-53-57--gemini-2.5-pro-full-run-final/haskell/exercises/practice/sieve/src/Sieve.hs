module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.Array (Array, listArray, (!), (//), assocs)

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = [] -- No primes less than 2
  | otherwise = map fst $ filter snd $ assocs $ sieve n -- Extract indices where value is True

-- Helper function to perform the sieving process using an array
sieve :: Integer -> Array Integer Bool
sieve n = loop 2 initialArray
  where
    -- Create an array representing numbers from 2 to n, initially all True (potential primes)
    initialArray :: Array Integer Bool
    initialArray = listArray (2, n) (repeat True)

    -- Recursive loop function to mark multiples
    -- p: current number being checked
    -- arr: the array tracking primality
    loop :: Integer -> Array Integer Bool -> Array Integer Bool
    loop p arr
      -- Optimization: Stop when p*p exceeds the limit n.
      -- All remaining numbers marked True must be prime.
      | p * p > n = arr
      -- If p is already marked as not prime (False), skip to the next number
      | not (arr ! p) = loop (p + 1) arr
      -- If p is prime (True), mark its multiples as not prime (False)
      | otherwise = loop (p + 1) updatedArr
      where
        -- Generate multiples of p starting from p*p up to n
        -- Example: if p=3, n=20, multiples = [9, 12, 15, 18]
        multiples = [p * p, p * p + p .. n]
        -- Create updates to mark these multiples as False in the array
        updates = [(m, False) | m <- multiples]
        -- Apply the updates to the array. (//) creates a new array with updates.
        updatedArr = arr // updates
