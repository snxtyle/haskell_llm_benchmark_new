module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
  let digits = filter isDigit xs
      cleanedDigits = stripCountryCode digits
  in if isValidNumber cleanedDigits
     then Just cleanedDigits
     else Nothing

-- Remove country code if present (only '1' is valid)
stripCountryCode :: String -> String
stripCountryCode ds
  | length ds == 11 && head ds == '1' = tail ds
  | otherwise = ds

-- Validate the phone number
isValidNumber :: String -> Bool
isValidNumber ds =
  length ds == 10 &&
  isValidAreaCode (take 3 ds) &&
  isValidExchangeCode (take 3 $ drop 3 ds)

-- Area code: first digit must be 2-9
isValidAreaCode :: String -> Bool
isValidAreaCode (d:_) = d `elem` ['2'..'9']
isValidAreaCode _ = False

-- Exchange code: first digit must be 2-9
isValidExchangeCode :: String -> Bool
isValidExchangeCode (d:_) = d `elem` ['2'..'9']
isValidExchangeCode _ = False
