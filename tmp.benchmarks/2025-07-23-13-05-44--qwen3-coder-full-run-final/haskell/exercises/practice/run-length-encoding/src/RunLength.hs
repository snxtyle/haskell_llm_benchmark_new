module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode (c:cs)
  | isDigit c = 
      let (digits, rest) = span isDigit (c:cs)
          count = read digits
          char = head rest
          remaining = tail rest
      in replicate count char ++ decode remaining
  | otherwise = c : decode cs

encode :: String -> String
encode [] = []
encode str = 
  let (firstGroup, remaining) = span (== head str) str
      count = length firstGroup
      encodedFirst = if count == 1 
                     then [head str] 
                     else show count ++ [head str]
  in encodedFirst ++ encode remaining
