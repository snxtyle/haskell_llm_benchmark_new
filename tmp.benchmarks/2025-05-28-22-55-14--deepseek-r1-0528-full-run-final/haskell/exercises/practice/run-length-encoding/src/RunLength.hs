module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode s@(c:cs)
  | isDigit c = 
      let (digits, rest) = span isDigit s
          count = read digits
          (char:rest') = rest
      in replicate count char ++ decode rest'
  | otherwise = c : decode cs

encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup xs@(x:_) 
      | n == 1    = [x]
      | otherwise = show n ++ [x]
      where n = length xs
