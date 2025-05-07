module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just $ primes !! (n - 1)

-- Infinite list of primes via a simple sieve of Eratosthenes
primes :: [Integer]
primes = sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve []     = []  -- should never happen
