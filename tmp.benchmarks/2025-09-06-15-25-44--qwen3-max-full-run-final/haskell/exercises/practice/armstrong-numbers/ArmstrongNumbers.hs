module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sumOfPowers
  where
    digits = toDigits n
    numDigits = length digits
    sumOfPowers = sum [d ^ numDigits | d <- digits]

-- Helper function to convert a number to a list of its digits
toDigits :: Integral a => a -> [a]
toDigits 0 = [0]
toDigits n = reverse (go (abs n))
  where
    go 0 = []
    go x = let (q, r) = x `divMod` 10 in r : go q
