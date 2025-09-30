module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sumOfPowers
  where
    digits = toDigits n
    numDigits = length digits
    sumOfPowers = sum [d ^ numDigits | d <- digits]

toDigits :: Integral a => a -> [a]
toDigits 0 = [0]
toDigits n = reverse (go n)
  where
    go 0 = []
    go x = (x `mod` 10) : go (x `div` 10)
