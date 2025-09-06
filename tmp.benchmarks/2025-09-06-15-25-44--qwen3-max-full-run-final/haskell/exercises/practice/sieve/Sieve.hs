module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
    | n < 2 = []
    | otherwise = sieve [2..n]
  where
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, not (x `elem` multiples)]
      where
        multiples = takeWhile (<= n) [p * i | i <- [2..]]
