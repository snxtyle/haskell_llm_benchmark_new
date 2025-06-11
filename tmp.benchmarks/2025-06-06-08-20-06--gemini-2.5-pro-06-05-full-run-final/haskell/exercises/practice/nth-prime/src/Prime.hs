module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))

primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]
  where
    isPrime k = all (\p -> k `mod` p /= 0) (takeWhile (\p -> p * p <= k) primes)
