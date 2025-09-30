module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just (primes !! (n - 1))

-- Infinite list of prime numbers using trial division
primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]

-- Check if a number is prime by testing divisibility
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)
