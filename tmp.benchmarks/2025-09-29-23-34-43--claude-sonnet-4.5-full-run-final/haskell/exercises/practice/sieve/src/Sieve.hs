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
    sieve (p:xs) = p : sieve (markMultiples p xs)
    
    -- Mark (remove) all multiples of p from the list
    markMultiples :: Integer -> [Integer] -> [Integer]
    markMultiples p = filter (not . isMultipleOf p)
    
    -- Check if y is a multiple of x by repeatedly subtracting x from y
    isMultipleOf :: Integer -> Integer -> Bool
    isMultipleOf x y = go y
      where
        go z
          | z == x    = True
          | z < x     = False
          | otherwise = go (z - x)
