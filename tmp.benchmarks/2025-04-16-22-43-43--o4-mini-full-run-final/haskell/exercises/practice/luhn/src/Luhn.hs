module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
  let stripped = filter (/= ' ') n
  in if length stripped <= 1 || any (not . isDigit) stripped
       then False
       else let digits    = map digitToInt stripped
                revDigits = reverse digits
                transform i d
                  | even i    = let dbl = d * 2
                                in if dbl > 9 then dbl - 9 else dbl
                  | otherwise = d
                total     = sum $ zipWith transform [1..] revDigits
             in total `mod` 10 == 0
