module Prime (nth) where

-- Helper function to check if a number is prime
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = not $ any divides [3,5..limit]
  where
    divides x = n `mod` x == 0
    limit = floor (sqrt (fromIntegral n :: Double))

-- Infinite list of prime numbers
primes :: [Integer]
primes = filter isPrime [2..]

-- Returns the nth prime (0-based index)
nth :: Int -> Maybe Integer
nth n
  | n < 0     = Nothing
  | otherwise = Just (primes !! n)
