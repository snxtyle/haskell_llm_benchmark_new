module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve []     = []
    sieve (p:xs) = p : sieve (filter (not . divides p) xs)

-- divides p x returns True if p divides x, by repeated subtraction
divides :: Integer -> Integer -> Bool
divides p x
  | x < p     = False
  | x == p    = True
  | otherwise = divides p (x - p)
