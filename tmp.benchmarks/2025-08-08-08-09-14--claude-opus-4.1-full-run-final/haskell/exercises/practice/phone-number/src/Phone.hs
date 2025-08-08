module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let digits = filter isDigit xs
        len = length digits
    in case len of
        10 -> validateNANP digits
        11 -> if head digits == '1' 
              then validateNANP (tail digits)
              else Nothing
        _ -> Nothing

validateNANP :: String -> Maybe String
validateNANP digits
    | length digits /= 10 = Nothing
    | not (validNXX (take 3 digits)) = Nothing  -- area code
    | not (validNXX (take 3 (drop 3 digits))) = Nothing  -- exchange code
    | otherwise = Just digits
  where
    validNXX (n:_) = n >= '2' && n <= '9'
    validNXX _ = False
