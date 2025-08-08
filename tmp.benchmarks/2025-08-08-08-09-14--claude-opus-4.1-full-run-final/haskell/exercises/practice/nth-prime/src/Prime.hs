module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just (primes !! (n - 1))
  where
    primes = sieve [2..]
    
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (p:xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)
