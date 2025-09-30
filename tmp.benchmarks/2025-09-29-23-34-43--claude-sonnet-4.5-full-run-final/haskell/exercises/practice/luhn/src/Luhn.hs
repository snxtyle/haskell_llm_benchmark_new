module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = 
  let stripped = filter (/= ' ') n
  in length stripped > 1 
     && all isDigit stripped 
     && checkSum stripped `mod` 10 == 0

checkSum :: String -> Int
checkSum s = sum $ zipWith ($) (cycle [id, double]) (map digitToInt (reverse s))
  where
    double x = let d = x * 2 in if d > 9 then d - 9 else d
