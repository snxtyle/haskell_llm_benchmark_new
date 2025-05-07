module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

-- | Check if a string is valid per the Luhn formula.
isValid :: String -> Bool
isValid n =
    let cleaned = filter (/= ' ') n
    in if length cleaned <= 1 || not (all isDigit cleaned)
       then False
       else
           let digits = map digitToInt $ reverse cleaned
               -- Process digits based on their position (0-indexed from the right)
               luhnProcessed = zipWith processDigit [0..] digits
           in sum luhnProcessed `mod` 10 == 0

-- | Process a single digit based on its index (0-based from the right).
-- Double digits at odd indices (second, fourth, etc. from the right).
-- If doubling results in a value > 9, subtract 9.
processDigit :: Int -> Int -> Int
processDigit index digit
    | odd index = let doubled = digit * 2
                  in if doubled > 9 then doubled - 9 else doubled
    | otherwise = digit -- Keep digits at even indices (first, third, etc. from the right) as is
