module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = 
    let stripped = filter (/= ' ') n
        onlyDigits = all isDigit stripped
        len = length stripped
    in len > 1 && onlyDigits && luhnCheck stripped

luhnCheck :: String -> Bool
luhnCheck str = 
    let digits = map digitToInt str
        reversed = reverse digits
        processed = zipWith processDigit [0..] reversed
        total = sum processed
    in total `mod` 10 == 0

processDigit :: Int -> Int -> Int
processDigit index digit
    | odd index = doubleAndAdjust digit
    | otherwise = digit

doubleAndAdjust :: Int -> Int
doubleAndAdjust n = 
    let doubled = n * 2
    in if doubled > 9 then doubled - 9 else doubled
