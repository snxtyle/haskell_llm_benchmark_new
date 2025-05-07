module Prime (nth) where

-- | Checks if a given integer is prime.
isPrime :: Integer -> Bool
isPrime k
  | k <= 1    = False  -- Numbers less than or equal to 1 are not prime
  | k == 2    = True   -- 2 is the first prime
  | even k    = False  -- Even numbers greater than 2 are not prime
  | otherwise = all (\d -> k `mod` d /= 0) $ takeWhile (\d -> d*d <= k) [3, 5..]
  -- Check divisibility only by odd numbers up to the square root of k

-- | Generates an infinite list of prime numbers.
primes :: [Integer]
primes = 2 : filter isPrime [3, 5..] -- Start with 2, then filter odd numbers

-- | Returns the nth prime number.
nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing -- There is no 0th or negative prime
  | otherwise = Just (primes !! (n - 1)) -- Access the (n-1)th element (0-based index)
