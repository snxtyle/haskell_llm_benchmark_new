module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n =
  let original = fromIntegral n :: Integer  -- Convert input to Integer for comparison
      str = show n                          -- Convert number to string to get digits
      numDigits = length str                -- Get the number of digits
      digits = map (fromIntegral . digitToInt) str  -- Convert string digits to [Integer]
      powered = map (\d -> d ^ numDigits) digits    -- Raise each digit to the power of numDigits
      sumPowered = sum powered                 -- Sum the powered digits
  in sumPowered == original                  -- Compare sum to original number
