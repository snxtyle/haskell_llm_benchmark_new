module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  let digits = filter isDigit xs
      cleaned = case digits of
        ('1':rest) -> rest
        _ -> digits
  in if validNumber cleaned then Just cleaned else Nothing

validNumber :: String -> Bool
validNumber s =
  length s == 10 &&
  validN (s !! 0) &&
  validN (s !! 3) &&
  all isDigit s

validN :: Char -> Bool
validN c = c >= '2' && c <= '9'
