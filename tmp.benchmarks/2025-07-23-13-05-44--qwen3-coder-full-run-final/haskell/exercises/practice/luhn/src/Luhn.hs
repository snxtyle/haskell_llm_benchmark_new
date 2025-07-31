module Luhn (isValid) where

isValid :: String -> Bool
isValid str = 
  let cleaned = filter (/= ' ') str
  in if length cleaned <= 1 || not (all isDigit cleaned)
       then False
       else sumDigits (reverse cleaned) 1 `mod` 10 == 0
  where
    isDigit c = c >= '0' && c <= '9'
    sumDigits [] _ = 0
    sumDigits (x:xs) pos =
      let digit = read [x]
          processedDigit = if pos `mod` 2 == 0 
                          then if digit * 2 > 9 then digit * 2 - 9 else digit * 2
                          else digit
      in processedDigit + sumDigits xs (pos + 1)
