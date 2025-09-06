module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode (c:cs)
  | isDigit c = let (digits, rest) = span isDigit (c:cs)
                    count = read digits :: Int
                    (char:rest') = rest
                in replicate count char ++ decode rest'
  | otherwise = c : decode cs

encode :: String -> String
encode = concatMap (\g -> if length g > 1 then show (length g) ++ [head g] else [head g]) . group
