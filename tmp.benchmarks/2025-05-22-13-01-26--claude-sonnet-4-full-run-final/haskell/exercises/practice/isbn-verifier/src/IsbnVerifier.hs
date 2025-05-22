module IsbnVerifier (isbn) where

import Data.Char (isDigit)

isbn :: String -> Bool
isbn input = 
    case parseIsbn (filter (/= '-') input) of
        Nothing -> False
        Just digits -> isValidChecksum digits

-- Parse the ISBN string into a list of digit values
parseIsbn :: String -> Maybe [Int]
parseIsbn s
    | length s /= 10 = Nothing
    | not (all isValidChar (take 9 s)) = Nothing
    | not (isValidCheckChar (last s)) = Nothing
    | otherwise = Just (map charToDigit s)
  where
    isValidChar c = isDigit c
    isValidCheckChar c = isDigit c || c == 'X'
    
    charToDigit c
        | isDigit c = read [c]
        | c == 'X' = 10
        | otherwise = 0  -- This shouldn't happen given our validation

-- Check if the weighted sum mod 11 equals 0
isValidChecksum :: [Int] -> Bool
isValidChecksum digits = 
    let weights = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
        weightedSum = sum (zipWith (*) digits weights)
    in weightedSum `mod` 11 == 0
