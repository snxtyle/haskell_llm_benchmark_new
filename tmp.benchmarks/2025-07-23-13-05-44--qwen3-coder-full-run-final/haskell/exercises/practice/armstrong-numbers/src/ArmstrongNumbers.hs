module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n = 
  let digits = map digitToInt $ show n
      numDigits = length digits
      sumOfPowers = sum $ map (^ numDigits) digits
  in fromIntegral sumOfPowers == (fromIntegral n :: Integer)
