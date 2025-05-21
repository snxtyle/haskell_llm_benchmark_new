module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
import Data.Array.ST    -- For mutable arrays in ST monad
import Control.Monad.ST -- For runST
import Data.Array       -- For freezing STArray to Array and extracting elements

primesUpTo :: Integer -> [Integer]
primesUpTo n
    | n < 2 = [] -- No primes less than 2
    | otherwise =
        runST $ do
            -- Create a mutable boolean array, indexed from 2 to n, initialized to True
            -- `newArray (low, high) initialValue`
            arr <- newArray (2, n) True :: ST s (STArray s Integer Bool)

            -- The sieve algorithm iterates up to sqrt(n)
            let limit = floor (sqrt (fromIntegral n :: Double))

            -- Sieve loop: p is the current number being checked for primality
            let sieveLoop p
                    | p > limit = return () -- Base case: p exceeds sqrt(n), all primes found
                    | otherwise = do
                        is_prime <- readArray arr p -- Check if p is marked as prime
                        if is_prime
                            then do
                                -- If p is prime, mark all its multiples as not prime
                                -- Start marking from p*p, as smaller multiples (p*2, p*3, etc.)
                                -- would have already been marked by smaller primes (2, 3, etc.).
                                let markMultiples m
                                        | m > n = return () -- Base case: multiple exceeds n
                                        | otherwise = do
                                            writeArray arr m False -- Mark as not prime
                                            markMultiples (m + p) -- Move to the next multiple
                                markMultiples (p * p)
                            else return () -- p is not prime, skip it

                        sieveLoop (p + 1) -- Move to the next number

            sieveLoop 2 -- Start the sieve process from 2

            -- After the sieve is complete, extract the prime numbers
            -- Freeze the mutable STArray into an immutable Array
            finalArr <- freeze arr
            -- Collect all indices (numbers) that are still marked True
            return [i | (i, True) <- assocs finalArr]
