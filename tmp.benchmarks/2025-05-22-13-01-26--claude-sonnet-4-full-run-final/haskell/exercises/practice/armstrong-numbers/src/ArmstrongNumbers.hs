module ArmstrongNumbers (armstrong) where

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n = n == sum [fromIntegral (read [d]) ^ numDigits | d <- show (abs n)]
  where
    numDigits = length (show (abs n))
