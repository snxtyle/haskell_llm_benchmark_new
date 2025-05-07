module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n = n == sumOfPowers
  where
    s = show n
    numDigits = length s
    digits = map digitToInt s
    sumOfPowers = sum $ map (\d -> fromIntegral d ^ numDigits) digits

-- Note: We need fromIntegral because d is Int (from digitToInt)
-- but the result of (^) needs to match the type of n (Integral a).
-- The sum also needs to match the type of n.
-- Alternatively, we could constrain 'a' to be 'Integer' or 'Int'
-- if we didn't want to deal with fromIntegral, but the original
-- type signature is more general (Integral a).
