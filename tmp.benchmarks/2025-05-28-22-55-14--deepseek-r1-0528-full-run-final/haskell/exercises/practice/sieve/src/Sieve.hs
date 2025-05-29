module Sieve (primesUpTo) where

import Data.Array (Array, listArray, (!), (//))
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = 
      let
        arr = listArray (2, n) (repeat True)
        arr' = markComposites arr 2 n
      in [p | p <- [2..n], arr' ! p]

markComposites :: Array Integer Bool -> Integer -> Integer -> Array Integer Bool
markComposites arr p n
  | p*p > n   = arr
  | arr ! p   = 
      let multiples = [p*p, p*p+p .. n]
          newArr = arr // [(i, False) | i <- multiples]
      in markComposites newArr (p+1) n
  | otherwise = markComposites arr (p+1) n
