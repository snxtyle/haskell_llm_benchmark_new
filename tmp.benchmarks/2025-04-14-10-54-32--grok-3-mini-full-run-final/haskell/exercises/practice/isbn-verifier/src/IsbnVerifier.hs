module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn str =
    let cleaned = filter (/= '-') str  -- Remove hyphens
    in if length cleaned /= 10 then False  -- Must be exactly 10 characters after cleaning
       else
           -- Check if characters are valid: first 9 must be digits, last can be digit or 'X'
           let isValidChar i c
                 | i < 9     = isDigit c
                 | otherwise = isDigit c || c == 'X'  -- Last character can be 'X'
               validChars = all (\(i, c) -> isValidChar i c) (zip [0..9] cleaned)
           in if not validChars then False
              else
                  -- Convert to values and compute weighted sum
                  let values = [ if i < 9 then digitToInt (cleaned !! i)  -- First 9: digit to int
                                 else if cleaned !! i == 'X' then 10  -- Last: 'X' as 10
                                 else digitToInt (cleaned !! i)  -- Last: digit to int
                               | i <- [0..9] ]
                      weightedSum = sum [ values !! i * (10 - i) | i <- [0..9] ]  -- Apply weights
                  in weightedSum `mod` 11 == 0  -- Check if sum mod 11 == 0
