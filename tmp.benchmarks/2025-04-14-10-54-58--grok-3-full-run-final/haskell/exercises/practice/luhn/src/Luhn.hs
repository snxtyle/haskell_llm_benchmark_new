module Luhn (isValid) where

isValid :: String -> Bool
isValid n = 
    let cleaned = filter (/= ' ') n
    in length cleaned > 1 && 
       all (`elem` "0123456789") cleaned &&
       (sumDigits . doubleEverySecond . reverse $ map charToInt cleaned) `mod` 10 == 0
  where
    charToInt c = read [c] :: Int
    doubleEverySecond = zipWith (*) (cycle [1, 2])
    sumDigits = sum . map (\x -> if x > 9 then x - 9 else x)
