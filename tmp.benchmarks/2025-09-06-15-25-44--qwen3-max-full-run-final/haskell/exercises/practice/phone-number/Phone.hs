module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let cleaned = filter isDigit xs
        withoutCountryCode = if length cleaned == 11 && head cleaned == '1' 
                             then tail cleaned 
                             else cleaned
    in if isValidNANP withoutCountryCode
       then Just withoutCountryCode
       else Nothing

isValidNANP :: String -> Bool
isValidNANP digits = 
    length digits == 10 &&
    isNChar (digits !! 0) &&
    isNChar (digits !! 3)
  where
    isNChar c = c >= '2' && c <= '9'
