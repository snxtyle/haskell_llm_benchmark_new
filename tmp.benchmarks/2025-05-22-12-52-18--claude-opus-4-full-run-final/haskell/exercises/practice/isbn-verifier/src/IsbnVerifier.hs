module IsbnVerifier (isbn) where

import Data.Char (isDigit)

isbn :: String -> Bool
isbn input = 
    let cleaned = filter (/= '-') input
        isValidChar c = isDigit c || c == 'X'
        isValidFormat = length cleaned == 10 && 
                       all isDigit (take 9 cleaned) &&
                       isValidChar (last cleaned)
    in if isValidFormat
       then checkSum cleaned == 0
       else False

checkSum :: String -> Int
checkSum digits = sum (zipWith (*) values weights) `mod` 11
  where
    values = map charToValue digits
    weights = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    charToValue c
        | isDigit c = read [c] :: Int
        | c == 'X'  = 10
        | otherwise = 0
