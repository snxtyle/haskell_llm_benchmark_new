module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = 
    let cleaned = filter (/= ' ') n
    in length cleaned > 1
       && all isDigit cleaned
       && luhnSum cleaned `mod` 10 == 0

-- Calculate the Luhn sum
luhnSum :: String -> Int
luhnSum s = sum $ zipWith processDigit (reverse s) [1..]
  where
    processDigit c pos
      | pos `mod` 2 == 1 = digitToInt c  -- Odd positions (from right) unchanged
      | otherwise = let doubled = digitToInt c * 2
                    in if doubled > 9 then doubled - 9 else doubled
