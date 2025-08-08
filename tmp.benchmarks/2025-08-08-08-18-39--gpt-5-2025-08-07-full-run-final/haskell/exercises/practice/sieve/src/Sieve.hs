module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2 .. n]
  where
    sieve [] = []
    sieve (p:xs)
      | p * p > n = p : xs
      | otherwise = p : sieve (xs `minus` multiplesFrom p)

    multiplesFrom :: Integer -> [Integer]
    multiplesFrom p = [p * p, p * p + p .. n]

    -- Remove the elements of the second sorted list from the first sorted list.
    minus :: [Integer] -> [Integer] -> [Integer]
    minus as [] = as
    minus [] _  = []
    minus (a:as) (b:bs)
      | a < b     = a : minus as (b:bs)
      | a == b    =     minus as bs
      | otherwise =     minus (a:as) bs
