module Sieve (primesUpTo) where

import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, not (isMultipleOf p x)]

    isMultipleOf :: Integer -> Integer -> Bool
    isMultipleOf p x
      | p > x     = False  -- p cannot divide x if p is larger than x
      | otherwise = go x p  -- Use repeated subtraction to check
      where
        go y divisor
          | y < divisor = y == 0  -- After subtracting, if remainder is 0, it's a multiple
          | otherwise   = go (y - divisor) divisor
