module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
  let stripped = filter (/= ' ') n
  in length stripped > 1
     && all isDigit stripped
     && luhnSum stripped `mod` 10 == 0

luhnSum :: String -> Int
luhnSum = sum . zipWith process [0..] . reverse
  where
    process :: Int -> Char -> Int
    process idx ch =
      let d = digitToInt ch
      in if odd idx
           then let x = d * 2 in if x > 9 then x - 9 else x
           else d
