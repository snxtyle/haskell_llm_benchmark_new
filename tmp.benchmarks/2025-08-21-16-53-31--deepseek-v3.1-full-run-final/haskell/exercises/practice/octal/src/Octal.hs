module Octal (readOct, showOct) where

readOct :: String -> Integer
readOct s = if isValidOctal s then convertOctal s 0 else 0
  where
    isValidOctal :: String -> Bool
    isValidOctal = all (\c -> c >= '0' && c <= '7')
    
    convertOctal :: String -> Integer -> Integer
    convertOctal [] acc = acc
    convertOctal (x:xs) acc
        | x >= '0' && x <= '7' = 
            let digitValue = toInteger (fromEnum x - fromEnum '0')
                newAcc = acc * 8 + digitValue
            in newAcc `seq` convertOctal xs newAcc
        | otherwise = 0

showOct :: Integer -> String
showOct n
    | n < 0 = '-' : showOct' (-n) []
    | otherwise = showOct' n []
  where
    showOct' :: Integer -> String -> String
    showOct' 0 acc = if null acc then "0" else acc
    showOct' num acc =
        let remainder = num `mod` 8
            quotient = num `div` 8
            digit = toEnum (fromEnum '0' + fromInteger remainder)
        in quotient `seq` remainder `seq` showOct' quotient (digit : acc)
