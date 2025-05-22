module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = 
  let stripped = filter (/= ' ') n
  in length stripped > 1 && 
     all isDigit stripped && 
     luhnCheck stripped

luhnCheck :: String -> Bool
luhnCheck digits = 
  let digitList = map digitToInt digits
      doubled = zipWith ($) (cycle [id, doubleAndReduce]) (reverse digitList)
      total = sum doubled
  in total `mod` 10 == 0

doubleAndReduce :: Int -> Int
doubleAndReduce x = 
  let doubled = x * 2
  in if doubled > 9 then doubled - 9 else doubled
