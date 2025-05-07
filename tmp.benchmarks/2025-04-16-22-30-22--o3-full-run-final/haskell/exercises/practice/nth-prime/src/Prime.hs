module Prime (nth) where

-- | Infinite lazy list of prime numbers.
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

-- | Predicate to check if a number is prime.
--   A number is prime if it is not divisible by any prime
--   less than or equal to its square root.
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = all (\p -> n `mod` p /= 0) $
                  takeWhile (\p -> p * p <= n) primes

-- | Return the n‑th prime (1‑based index).
--   Returns Nothing for n <= 0.
nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just $ primes !! (n - 1)
