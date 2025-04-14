module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.Array.ST (STUArray, newArray, readArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, (!))
import Control.Monad (forM_, when)

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve n
  where
    sieve :: Integer -> [Integer]
    sieve n = runSTUArray $ do
      -- Create a boolean array "isPrime[0..n]" and initialize
      -- all entries it as true. A value in isPrime[i] will
      -- finally be false if i is Not a prime, else true.
      isPrime <- newArray (0, fromIntegral n) True :: STUArray s Int Bool
      writeArray isPrime 0 False
      writeArray isPrime 1 False
      
      -- Use Sieve of Eratosthenes to mark non-prime numbers as false
      forM_ [2 .. floor (sqrt (fromIntegral n))] $ \i -> do
        prime <- readArray isPrime i
        when prime $ do
          -- Update all multiples of i starting from i*i
          forM_ [i*i, i*i+i .. fromIntegral n] $ \j -> do
            writeArray isPrime j False
      
      -- Create the list of all prime numbers
      let primes = filterM (\i -> readArray isPrime i) [2 .. fromIntegral n]
      return primes
    
    filterM p = foldr (\x xs -> do b <- p x; if b then return (x:xs) else xs) (return [])
