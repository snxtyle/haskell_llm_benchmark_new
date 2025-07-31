module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, not (isMultiple p x)]
    
    isMultiple p x = p <= x && any (== x) [p*p, p*p+p .. n]
