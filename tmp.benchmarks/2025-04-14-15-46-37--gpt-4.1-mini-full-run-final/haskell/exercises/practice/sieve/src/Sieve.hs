module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve (filter (notMultiple p) xs)

    -- Check if x is not a multiple of p without using division or modulo
    notMultiple p x = not (isMultiple p x)

    -- Determine if x is a multiple of p by repeated subtraction
    isMultiple p x
      | x < p     = False
      | x == p    = True
      | otherwise = isMultiple p (x - p)
