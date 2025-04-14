module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n < 1     = Nothing
    | otherwise = Just (nthPrime n)

-- Function to find the nth prime number
nthPrime :: Int -> Integer
nthPrime n = head (drop (n - 1) primes)

-- Infinite list of prime numbers
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)
