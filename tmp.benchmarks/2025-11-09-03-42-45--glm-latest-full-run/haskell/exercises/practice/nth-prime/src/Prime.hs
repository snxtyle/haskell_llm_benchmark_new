module Prime (nth) where

-- | Sieve of Eratosthenes
-- Takes a list of integers and returns a list of primes.
sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- | Infinite list of all prime numbers.
primes :: [Integer]
primes = sieve [2..]

-- | Returns the nth prime number.
-- If n is less than 1, returns Nothing.
nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just (primes !! (n - 1))
