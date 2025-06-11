module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = 
    let clean = filter (/= ' ') n
    in if length clean <= 1 || not (all isDigit clean)
       then False
       else 
           let digits = map digitToInt clean
               reversed = reverse digits
               processed = map (\(i, d) -> 
                           if odd i 
                           then let doubled = d * 2 in if doubled > 9 then doubled - 9 else doubled
                           else d) 
                         (zip [0..] reversed)
               total = sum processed
           in total `mod` 10 == 0
