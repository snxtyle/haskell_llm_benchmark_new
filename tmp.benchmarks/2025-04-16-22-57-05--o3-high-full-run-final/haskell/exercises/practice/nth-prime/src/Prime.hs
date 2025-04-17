module Prime (nth) where

-- | Returns the n‑th prime (1‑indexed).
--   For non‑positive inputs it returns Nothing.
--
--   Examples:
--
--   >>> nth 1
--   Just 2
--
--   >>> nth 6
--   Just 13
--
--   >>> nth 0
--   Nothing
nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just $ primes !! (n - 1)

-- | Infinite, lazily generated list of prime numbers.
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

-- | Simple primality test using trial division by already
--   discovered primes, stopping when the divisor’s square
--   exceeds the candidate number.
isPrime :: Integer -> Bool
isPrime k =
  all (\p -> k `mod` p /= 0) $
  takeWhile (\p -> p * p <= k) primes
