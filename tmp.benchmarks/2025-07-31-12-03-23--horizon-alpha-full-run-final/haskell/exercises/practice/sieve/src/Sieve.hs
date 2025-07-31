module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, assocs)
import Data.Array.Base (unsafeFreeze)
import Data.Ix (inRange)

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2     = []
  | otherwise = map toInteger $ collectPrimes sieveArr
  where
    upper :: Int
    upper = fromInteger n

    sieveArr :: UArray Int Bool
    sieveArr = runST (sieve upper)

    collectPrimes :: UArray Int Bool -> [Int]
    collectPrimes arr = [ i | (i, True) <- assocs arr ]

sieve :: Int -> ST s (UArray Int Bool)
sieve upper = do
  -- Array over [2..upper], True means "potentially prime"
  a :: STUArray s Int Bool <- newArray (2, upper) True

  let limit :: Int
      -- Integer square root without division: increment k while k*k <= upper
      limit = isqrt upper

      -- Integer sqrt via monotone increasing k^2 without division/remainder
      isqrt :: Int -> Int
      isqrt m = go 0
        where
          go k = if (k + 1) * (k + 1) <= m then go (k + 1) else k

      markMultiples :: Int -> ST s ()
      markMultiples p = do
        -- start at p*p, step by p, stay within bounds
        let start = p * p
        when (inRange (2, upper) start) $
          forM_ [start, start + p .. upper] $ \j ->
            writeArray a j False

  -- Main sieve loop for p in [2..limit]
  forM_ [2 .. limit] $ \p -> do
    isCand <- readArray a p
    when isCand (markMultiples p)

  unsafeFreeze a
