module IsbnVerifier (isbn) where

import Data.Char (isDigit)

isbn :: String -> Bool
isbn input = isValidFormat cleaned && isValidChecksum cleaned
  where
    cleaned = filter (/= '-') input

isValidFormat :: String -> Bool
isValidFormat s = 
    length s == 10 &&
    all isDigit (take 9 s) &&
    (isDigit (last s) || last s == 'X')

isValidChecksum :: String -> Bool
isValidChecksum s
    | not (isValidFormat s) = False
    | otherwise = checksum `mod` 11 == 0
  where
    digits = map charToValue s
    weights = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    checksum = sum $ zipWith (*) digits weights

charToValue :: Char -> Int
charToValue 'X' = 10
charToValue c
    | isDigit c = read [c]
    | otherwise = 0
