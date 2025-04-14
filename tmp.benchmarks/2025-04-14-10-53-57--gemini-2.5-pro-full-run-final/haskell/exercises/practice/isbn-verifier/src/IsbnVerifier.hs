module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)
import Data.Maybe (isJust, mapMaybe)

-- Helper function to parse a character into a Maybe Int if it's a digit
parseDigit :: Char -> Maybe Int
parseDigit c
    | isDigit c = Just (digitToInt c)
    | otherwise = Nothing

-- Helper function to parse the check digit (last character)
parseCheckDigit :: Char -> Maybe Int
parseCheckDigit c
    | isDigit c = Just (digitToInt c)
    | c == 'X'  = Just 10
    | otherwise = Nothing

isbn :: String -> Bool
isbn s =
    -- Remove hyphens from the input string
    let cleaned = filter (/= '-') s
    -- Check if the cleaned string has exactly 10 characters
    in case cleaned of
         [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10] ->
             -- Try parsing the first 9 characters as digits
             let maybeFirst9Digits = mapM parseDigit [d1, d2, d3, d4, d5, d6, d7, d8, d9]
             -- Try parsing the last character as a check digit (digit or 'X')
                 maybeCheckDigit = parseCheckDigit d10
             in case (maybeFirst9Digits, maybeCheckDigit) of
                  -- If both parts parsed successfully
                  (Just first9Digits, Just checkDigit) ->
                      -- Combine the digits and calculate the weighted sum
                      let allDigits = first9Digits ++ [checkDigit]
                          weightedSum = sum $ zipWith (*) allDigits [10, 9 .. 1]
                      -- Check if the sum is divisible by 11
                      in weightedSum `mod` 11 == 0
                  -- If parsing failed for either part, the ISBN is invalid
                  _ -> False
         -- If the length is not 10 after removing hyphens, it's invalid
         _ -> False
