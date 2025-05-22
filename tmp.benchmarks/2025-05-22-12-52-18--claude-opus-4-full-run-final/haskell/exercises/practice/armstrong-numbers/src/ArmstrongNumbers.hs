module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n 
  | n < 0     = False
  | otherwise = n == sumOfPowers
  where
    digits = getDigits (abs n)
    numDigits = length digits
    sumOfPowers = sum [d ^ numDigits | d <- digits]
    
    getDigits :: Integral a => a -> [a]
    getDigits 0 = [0]
    getDigits x = reverse (getDigits' x)
      where
        getDigits' 0 = []
        getDigits' y = (y `mod` 10) : getDigits' (y `div` 10)
