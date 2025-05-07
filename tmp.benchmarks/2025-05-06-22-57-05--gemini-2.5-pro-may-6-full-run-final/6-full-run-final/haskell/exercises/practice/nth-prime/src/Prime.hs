module Prime (nth) where

-- Helper function to calculate integer square root
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromInteger

-- Helper function to check if a number is prime
-- This version is optimized for checking odd numbers, as 2 is handled separately.
isPrime :: Integer -> Bool
isPrime k
  | k <= 1    = False  -- Should not happen if we filter [3,5..]
  | k == 2    = True   -- Base case, though not directly used by filter [3,5..]
  | even k    = False  -- Should not happen if we filter [3,5..]
  | otherwise = 
      -- Check for divisibility by odd numbers from 3 up to sqrt(k)
      all (\divisor -> k `mod` divisor /= 0) [3, 5 .. isqrt k]

-- An infinite list of all prime numbers
allPrimes :: [Integer]
-- Start with 2, then filter odd numbers (starting from 3) using our isPrime helper.
allPrimes = 2 : filter isPrime [3,5..] 

nth :: Int -> Maybe Integer
nth n
  | n <= 0  = Nothing  -- Prime numbers are indexed from 1 (e.g., 1st prime, 2nd prime)
  | otherwise = Just (allPrimes !! (n - 1)) -- (n-1) because lists are 0-indexed
