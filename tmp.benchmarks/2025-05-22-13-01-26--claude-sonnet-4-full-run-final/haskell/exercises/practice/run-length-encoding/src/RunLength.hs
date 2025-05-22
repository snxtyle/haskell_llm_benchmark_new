module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode encodedText = decodeHelper encodedText
  where
    decodeHelper [] = []
    decodeHelper (c:cs)
      | isDigit c = 
          let (numStr, rest) = span isDigit (c:cs)
              count = read numStr
          in case rest of
               (char:remaining) -> replicate count char ++ decodeHelper remaining
               [] -> []
      | otherwise = c : decodeHelper cs

encode :: String -> String
encode [] = []
encode text = encodeHelper text
  where
    encodeHelper [] = []
    encodeHelper (c:cs) = 
      let (same, different) = span (== c) (c:cs)
          count = length same
      in if count == 1
         then c : encodeHelper different
         else show count ++ [c] ++ encodeHelper different
