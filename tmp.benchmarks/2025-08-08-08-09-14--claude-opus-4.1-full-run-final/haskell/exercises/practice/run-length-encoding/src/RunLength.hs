module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode "" = ""
decode encodedText = decodeHelper encodedText
  where
    decodeHelper [] = []
    decodeHelper (x:xs)
      | isDigit x = 
          let (digits, rest) = span isDigit (x:xs)
              count = read digits :: Int
          in case rest of
               (c:cs) -> replicate count c ++ decodeHelper cs
               []     -> []
      | otherwise = x : decodeHelper xs

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup [] = ""
    encodeGroup [x] = [x]
    encodeGroup xs@(x:_) = 
      let count = length xs
      in if count == 1
         then [x]
         else show count ++ [x]
