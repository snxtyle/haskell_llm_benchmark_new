module Prime (nth) where

isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\d -> n `mod` d /= 0) (takeWhile (\d -> d * d <= n) [3,5..])

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))
