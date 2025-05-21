module Prime (nth) where

-- | Checks if an Integer is a prime number.
isPrime :: Integer -> Bool
isPrime k
    | k <= 1    = False -- Numbers less than or equal to 1 are not prime
    | k == 2    = True  -- 2 is the only even prime number
    | even k    = False -- Other even numbers are not prime
    | otherwise = not $ any (\x -> k `mod` x == 0) [3, 5 .. floor (sqrt (fromIntegral k))]
    -- For odd numbers, check divisibility by odd numbers up to its square root.

-- | An infinite list of prime numbers.
primes :: [Integer]
primes = 2 : 3 : filter isPrime [5, 7 ..]
    -- Start with 2 and 3, then filter subsequent odd numbers using isPrime.

-- | Given a number n, determine what the nth prime is.
-- Returns Nothing if n is less than or equal to 0.
nth :: Int -> Maybe Integer
nth n
    | n <= 0    = Nothing -- The problem implies n should be a positive integer.
    | otherwise = Just (primes !! (n - 1)) -- Get the (n-1)th element from the 0-indexed primes list.
