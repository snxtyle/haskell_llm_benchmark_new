module Binary (toDecimal) where

-- Helper function to check if a character is a valid binary digit ('0' or '1').
isValidBinaryChar :: Char -> Bool
isValidBinaryChar '0' = True
isValidBinaryChar '1' = True
isValidBinaryChar _   = False

-- Helper function to convert a valid binary character to its integer value.
-- This function assumes the character is already validated as '0' or '1'.
charToBinDigit :: Char -> Int
charToBinDigit '0' = 0
charToBinDigit '1' = 1
-- The following case should ideally not be reached if input is pre-validated.
charToBinDigit c   = error ("Invalid binary digit passed to charToBinDigit: " ++ [c])

-- Converts a binary string to its decimal equivalent.
-- If the string contains any character other than '0' or '1',
-- it is considered invalid, and the function returns 0.
-- Otherwise, the conversion is done using Horner's method.
-- Example: "101" -> ((0*2 + 1)*2 + 0)*2 + 1 = 5.
-- An empty string "" will result in 0.
toDecimal :: String -> Int
toDecimal xs
  | not (all isValidBinaryChar xs) = 0
  | otherwise = foldl step 0 xs
  where
    step accumulator digitChar = accumulator * 2 + charToBinDigit digitChar
