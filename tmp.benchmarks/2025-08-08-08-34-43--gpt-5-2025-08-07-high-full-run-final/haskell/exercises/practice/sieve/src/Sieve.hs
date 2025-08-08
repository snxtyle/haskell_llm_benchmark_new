module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = sieve [2 .. n]
  where
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (p:xs)
      | p * p > n = p : xs
      | otherwise = p : sieve (xs `minus` [p * p, p * p + p .. n])

    -- Remove elements of the second sorted list from the first sorted list.
    minus :: [Integer] -> [Integer] -> [Integer]
    minus [] _ = []
    minus xs [] = xs
    minus xxs@(x:xs) yys@(y:ys)
      | x < y     = x : minus xs yys
      | x == y    = minus xs ys
      | otherwise = minus xxs ys
