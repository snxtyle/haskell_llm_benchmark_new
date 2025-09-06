module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
    | n < 2     = []
    | otherwise = sieve [2..n]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (p:xs) = p : sieve (removeMultiples p xs)

removeMultiples :: Integer -> [Integer] -> [Integer]
removeMultiples p xs = filter (not . isMultipleOf p) xs
  where
    isMultipleOf factor num = go factor
      where
        go current
          | current == num = True
          | current > num = False
          | otherwise = go (current + factor)
