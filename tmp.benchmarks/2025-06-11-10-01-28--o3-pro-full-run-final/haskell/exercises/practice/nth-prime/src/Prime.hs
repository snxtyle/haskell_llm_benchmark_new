module Prime (nth) where

-- | Determine the n-th prime (1-based indexing).
--   Returns 'Nothing' for non-positive inputs.
nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

-- Infinite list of prime numbers generated lazily.
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

-- Primality test using trial division by already discovered primes
-- up to the square root of the candidate.
isPrime :: Integer -> Bool
isPrime k
  | k < 2     = False
  | k == 2    = True
  | even k    = False
  | otherwise = not $ any divides $ takeWhile (\p -> p * p <= k) primes
  where
    divides p = k `mod` p == 0
