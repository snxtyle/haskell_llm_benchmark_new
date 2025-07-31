module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

-- Infinite list of prime numbers using trial division by prior primes.
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
  where
    isPrime :: Integer -> Bool
    isPrime x = all (\p -> x `mod` p /= 0) $ takeWhile (\p -> p * p <= x) primes
