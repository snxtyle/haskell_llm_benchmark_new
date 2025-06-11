module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n = n == sumOfPowers
  where
    s = show n
    power = length s
    digits = map (fromIntegral . digitToInt) s
    sumOfPowers = sum (map (^ power) digits)
