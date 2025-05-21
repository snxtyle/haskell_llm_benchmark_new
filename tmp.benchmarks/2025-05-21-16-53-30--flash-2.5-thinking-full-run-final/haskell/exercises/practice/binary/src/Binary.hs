module Binary (toDecimal) where

import Data.Char (digitToInt)

-- | Converts a binary string to its decimal equivalent.
-- Throws an error if the string is empty or contains non-binary digits.
toDecimal :: String -> Int
toDecimal [] = error "Invalid binary string: empty"
toDecimal s
    | all (\c -> c == '0' || c == '1') s =
        foldl (\acc c -> acc * 2 + digitToInt c) 0 s
    | otherwise = error "Invalid binary string: contains non-binary digits"
