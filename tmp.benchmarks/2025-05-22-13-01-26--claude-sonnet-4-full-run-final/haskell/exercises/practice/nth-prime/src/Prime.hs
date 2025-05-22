module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = Just (primes !! (n - 1))

-- Generate an infinite list of prime numbers
primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]

-- Check if a number is prime
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = not (any (\p -> n `mod` p == 0) (takeWhile (\p -> p * p <= n) primes))
