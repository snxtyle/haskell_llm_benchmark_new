module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

-- | Verify whether a given string is a valid ISBN‑10.
--   The string may contain hyphens; these are ignored when checking.
--   An uppercase 'X' is allowed only in the last position, where it
--   represents the value 10.
isbn :: String -> Bool
isbn input =
  case filter (/= '-') input of
    [a,b,c,d,e,f,g,h,i,j] ->
      let firstNineValid = all isDigit [a,b,c,d,e,f,g,h,i]
          lastValid      = isDigit j || j == 'X'
          digits         = map digitValue [a,b,c,d,e,f,g,h,i] ++ [digitValue j]
          weightedSum    = sum $ zipWith (*) digits [10,9..1]
      in  firstNineValid
          && lastValid
          && weightedSum `mod` 11 == 0
    _ -> False
  where
    digitValue :: Char -> Int
    digitValue 'X' = 10
    digitValue c   = digitToInt c
