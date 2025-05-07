module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

import Data.Array.ST
import Control.Monad
import Control.Monad.ST

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = runST $ do
      -- Create a mutable array from 2 to n, initialized to True
      arr <- newArray (2, n) True :: ST s (STUArray s Integer Bool)
      let
        -- For each i from 2 up to sqrt(n), if i is marked True, mark all multiples of i as False
        sieve i
          | i * i > n = return ()
          | otherwise = do
              isPrime <- readArray arr i
              when isPrime $ do
                -- Mark all multiples of i as not prime
                let markMultiples j
                      | j > n     = return ()
                      | otherwise = writeArray arr j False >> markMultiples (j + i)
                markMultiples (i + i)
              sieve (i + 1)
      sieve 2
      -- Collect all indices that are still True (i.e., primes)
      primes <- filterM (readArray arr) [2..n]
      return primes
