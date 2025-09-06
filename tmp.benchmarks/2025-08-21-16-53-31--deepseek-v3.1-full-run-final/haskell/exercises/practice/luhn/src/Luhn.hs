module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n
  | length cleaned <= 1 = False
  | not (all isDigit cleaned) = False
  | otherwise = (sumDigits processed) `mod` 10 == 0
  where
    cleaned = filter (/= ' ') n
    digits = map digitToInt cleaned
    processed = processDigits (reverse digits)
    
    processDigits :: [Int] -> [Int]
    processDigits [] = []
    processDigits [x] = [x]
    processDigits (x:y:xs) = x : (doubleAndAdjust y) : processDigits xs
    
    doubleAndAdjust :: Int -> Int
    doubleAndAdjust x
      | doubled > 9 = doubled - 9
      | otherwise = doubled
      where doubled = x * 2

    sumDigits :: [Int] -> Int
    sumDigits = sum
