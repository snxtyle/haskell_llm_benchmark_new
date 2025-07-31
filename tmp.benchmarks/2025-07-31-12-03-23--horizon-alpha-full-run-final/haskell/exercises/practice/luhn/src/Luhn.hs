module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
  let stripped = filter (/= ' ') n
  in  not (null stripped)
      && length stripped > 1
      && all isDigit stripped
      && luhnCheck stripped

luhnCheck :: String -> Bool
luhnCheck s =
  let digits = map digitToInt s
      -- process from rightmost, doubling every second digit
      processed = zipWith apply [0..] (reverse digits)
      total = sum processed
  in total `mod` 10 == 0

apply :: Int -> Int -> Int
apply idx d
  | odd idx   = let x = d * 2 in if x > 9 then x - 9 else x
  | otherwise = d
