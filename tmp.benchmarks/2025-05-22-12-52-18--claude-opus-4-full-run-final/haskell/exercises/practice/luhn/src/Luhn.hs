module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = 
    let stripped = filter (/= ' ') n
        onlyDigits = all isDigit stripped
        len = length stripped
    in len > 1 && onlyDigits && checkLuhn stripped

checkLuhn :: String -> Bool
checkLuhn str = 
    let digits = map digitToInt str
        reversed = reverse digits
        doubled = doubleEverySecond reversed
        sumDigits = sum doubled
    in sumDigits `mod` 10 == 0

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond [] = []
doubleEverySecond [x] = [x]
doubleEverySecond (x:y:xs) = x : doubleDigit y : doubleEverySecond xs

doubleDigit :: Int -> Int
doubleDigit n = 
    let doubled = n * 2
    in if doubled > 9 then doubled - 9 else doubled
