module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let cleaned = filter isDigit xs
        len = length cleaned
    in case len of
        10 -> validateNumber cleaned
        11 -> if head cleaned == '1' 
              then validateNumber (tail cleaned)
              else Nothing
        _  -> Nothing

validateNumber :: String -> Maybe String
validateNumber digits
    | length digits /= 10 = Nothing
    | not (isValidNPA (take 3 digits)) = Nothing
    | not (isValidExchange (take 3 (drop 3 digits))) = Nothing
    | otherwise = Just digits
  where
    isValidNPA (n:_) = n >= '2' && n <= '9'
    isValidNPA _ = False
    isValidExchange (n:_) = n >= '2' && n <= '9'
    isValidExchange _ = False
