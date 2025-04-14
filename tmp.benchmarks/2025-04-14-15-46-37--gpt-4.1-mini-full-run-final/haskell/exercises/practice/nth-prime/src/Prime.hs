module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n < 1     = Nothing
  | otherwise = Just (primes !! (n - 1))
  where
    primes = filter isPrime [2..]

    isPrime :: Integer -> Bool
    isPrime x = all (\d -> x `mod` d /= 0) [2..(floor . sqrt . fromIntegral) x]
