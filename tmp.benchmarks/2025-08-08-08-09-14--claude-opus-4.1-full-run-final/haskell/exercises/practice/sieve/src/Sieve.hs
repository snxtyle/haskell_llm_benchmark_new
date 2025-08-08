module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n 
  | n < 2     = []
  | otherwise = sieve [2..n]
  where
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (p:xs) = p : sieve (filter (notMultipleOf p) xs)
    
    notMultipleOf :: Integer -> Integer -> Bool
    notMultipleOf p x = not (isMultiple p x)
    
    -- Check if x is a multiple of p without using division
    -- x is a multiple of p if x = k * p for some k >= 2
    isMultiple :: Integer -> Integer -> Bool
    isMultiple p x = any (\k -> k * p == x) [2..x]
