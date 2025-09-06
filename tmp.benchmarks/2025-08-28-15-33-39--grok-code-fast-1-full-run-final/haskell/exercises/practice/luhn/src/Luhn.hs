module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid s =
  let cleaned = filter (/= ' ') s
  in if length cleaned <= 1
     then False
     else if not (all isDigit cleaned)
     then False
     else let digits = map digitToInt cleaned
              reversed = reverse digits
              processed = zipWith (\i d -> if i `mod` 2 == 1
                                           then let dd = d * 2
                                                in if dd > 9 then dd - 9 else dd
                                           else d) [0..] reversed
              total = sum processed
          in total `mod` 10 == 0
