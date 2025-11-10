module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n =
  let s = show n
      numDigits = length s
      digits = map digitToInt s
      armstrongSum = sum (map (^ numDigits) digits)
  in fromIntegral armstrongSum == n
