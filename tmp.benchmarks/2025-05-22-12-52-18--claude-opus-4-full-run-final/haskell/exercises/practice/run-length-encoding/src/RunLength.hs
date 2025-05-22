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
encode text = concatMap encodeRun (group text)
  where
    encodeRun [x] = [x]
    encodeRun xs = show (length xs) ++ [head xs]
