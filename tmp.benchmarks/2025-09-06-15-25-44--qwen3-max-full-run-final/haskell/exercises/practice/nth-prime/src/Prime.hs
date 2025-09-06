module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0 = Nothing
  | otherwise = Just (findNthPrime n 0 2)
  where
    findNthPrime :: Int -> Int -> Integer -> Integer
    findNthPrime target count current
      | count == target = current - 1
      | isPrime current = findNthPrime target (count + 1) (current + 1)
      | otherwise = findNthPrime target count (current + 1)
    
    isPrime :: Integer -> Bool
    isPrime x
      | x < 2 = False
      | x == 2 = True
      | even x = False
      | otherwise = not $ any (\d -> x `mod` d == 0) [3, 5 .. floor $ sqrt $ fromIntegral x]
