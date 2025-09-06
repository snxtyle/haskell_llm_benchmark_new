module IsbnVerifier (isbn) where

import Data.Char (isDigit)

isbn :: String -> Bool
isbn s
    | length clean /= 10 = False
    | not (all isValidChar clean) = False
    | not (isValidCheckDigit (last clean)) = False
    | any (== 'X') (init clean) = False  -- X can only be in the last position
    | otherwise = (sum (zipWith (*) [10,9..1] (map charToInt clean)) `mod` 11) == 0
    where
        clean = filter (/= '-') s
        isValidChar c = isDigit c || c == 'X'
        isValidCheckDigit c = isDigit c || c == 'X'
        charToInt 'X' = 10
        charToInt c = read [c]
