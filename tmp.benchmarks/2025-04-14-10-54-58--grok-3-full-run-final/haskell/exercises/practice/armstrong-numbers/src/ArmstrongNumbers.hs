module ArmstrongNumbers (armstrong) where

armstrong :: (Integral a, Read a, Show a) => a -> Bool
armstrong num = num == sumOfPowers
  where
    digits = map (read . (:[])) $ show num
    numDigits = length digits
    sumOfPowers = sum $ map (^ numDigits) digits
