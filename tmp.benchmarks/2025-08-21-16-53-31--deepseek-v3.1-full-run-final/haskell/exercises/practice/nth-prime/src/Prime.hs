module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
  | n <= 0    = Nothing
  | otherwise = Just (findNthPrime n 2 0)

findNthPrime :: Int -> Integer -> Int -> Integer
findNthPrime target current count
  | count == target = current - 1
  | isPrime current = findNthPrime target (current + 1) (count + 1)
  | otherwise       = findNthPrime target (current + 1) count

isPrime :: Integer -> Bool
isPrime n
  | n <= 1     = False
  | n == 2     = True
  | even n     = False
  | otherwise  = checkDivisors n 3 (floor (sqrt (fromIntegral n)))

checkDivisors :: Integer -> Integer -> Integer -> Bool
checkDivisors n divisor maxDivisor
  | divisor > maxDivisor = True
  | n `mod` divisor == 0 = False
  | otherwise = checkDivisors n (divisor + 2) maxDivisor
