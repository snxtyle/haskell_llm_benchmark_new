module IsbnVerifier (isbn) where

import Data.Char (isDigit)

isbn :: String -> Bool
isbn input = 
  let cleaned = filter (/= '-') input
  in length cleaned == 10 && isValidIsbn cleaned

isValidIsbn :: String -> Bool
isValidIsbn str =
  let first9 = take 9 str
      checkChar = str !! 9
  in all isDigit first9 && isValidCheckChar checkChar && checksumValid str

isValidCheckChar :: Char -> Bool
isValidCheckChar c = isDigit c || c == 'X'

checksumValid :: String -> Bool
checksumValid str =
  let digits = map charToValue (zip str [10, 9..1])
      total = sum digits
  in total `mod` 11 == 0

charToValue :: (Char, Int) -> Int
charToValue ('X', weight) = 10 * weight
charToValue (c, weight) = digitToInt c * weight

digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'
