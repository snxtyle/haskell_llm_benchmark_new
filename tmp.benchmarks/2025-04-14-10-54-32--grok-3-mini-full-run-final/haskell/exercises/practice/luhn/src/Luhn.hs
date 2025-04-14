module Luhn (isValid) where

import Data.Char (digitToInt, isDigit)

isValid :: String -> Bool
isValid s =
    let cleaned = filter (/= ' ') s  -- Remove spaces
    in if not (all isDigit cleaned) then False  -- Check for non-digit characters
       else if length cleaned <= 1 then False  -- Length check
       else
           let digits = map digitToInt cleaned  -- Convert to list of Ints
               n = length digits
               processed = [ if (n - i) `mod` 2 == 0  -- Double every second digit from the right
                             then let doubled = digits !! i * 2
                                  in if doubled > 9 then doubled - 9 else doubled
                             else digits !! i
                           | i <- [0..n-1] ]
               sumProcessed = sum processed
           in sumProcessed `mod` 10 == 0  -- Check if sum is divisible by 10
