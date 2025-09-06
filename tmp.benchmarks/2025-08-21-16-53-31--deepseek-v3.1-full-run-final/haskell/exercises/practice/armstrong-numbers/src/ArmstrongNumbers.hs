module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ numDigits) digits)
  where
    digits = getDigits n
    numDigits = length digits

getDigits :: Integral a => a -> [a]
getDigits 0 = [0]
getDigits n = getDigits' n []
  where
    getDigits' 0 acc = acc
    getDigits' num acc = getDigits' (num `div` 10) (num `mod` 10 : acc)
