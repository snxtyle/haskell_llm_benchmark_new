module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Control.Monad

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = runST $ do
      -- Create a mutable array where True means the number is prime (unmarked)
      -- Index 0 and 1 are not used, indices 2 to n represent the numbers
      arr <- newArray (2, fromInteger n) True :: ST s (STArray s Int Bool)
      
      -- Sieve algorithm
      forM_ [2..floor (sqrt (fromInteger n))] $ \i -> do
        isPrime <- readArray arr i
        when isPrime $ do
          -- Mark all multiples of i as composite (not prime)
          forM_ [i*i, i*i+i .. fromInteger n] $ \multiple -> do
            writeArray arr multiple False
      
      -- Collect all numbers that remain marked as prime
      primes <- forM [2..fromInteger n] $ \i -> do
        isPrime <- readArray arr i
        return $ if isPrime then Just (toInteger i) else Nothing
      
      return [p | Just p <- primes]
