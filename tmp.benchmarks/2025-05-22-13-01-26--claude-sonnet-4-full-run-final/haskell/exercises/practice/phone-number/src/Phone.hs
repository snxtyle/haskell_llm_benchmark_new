module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
  let digits = filter isDigit xs
      cleanedDigits = case digits of
        ('1':rest) -> if length rest == 10 then rest else digits
        _ -> digits
  in if isValidNANP cleanedDigits
     then Just cleanedDigits
     else Nothing

isValidNANP :: String -> Bool
isValidNANP digits = 
  length digits == 10 &&
  case digits of
    (a:n:x:e:_) -> a >= '2' && a <= '9' && n >= '2' && n <= '9' && e >= '2' && e <= '9'
    _ -> False
