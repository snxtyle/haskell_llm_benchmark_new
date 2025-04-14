module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (primes !! (n - 1))  -- primes is 0-based, so adjust for 1-based nth
  where
    primes = filter isPrime [2..]  -- Infinite list of primes

    isPrime :: Integer -> Bool
    isPrime k
      | k <= 1    = False  -- 1 and below are not prime
      | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral k))], k `mod` x == 0]  -- Check for divisors up to sqrt(k)
