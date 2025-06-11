module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode (c:cs)
   | isDigit c = 
       let (digits, rest) = span isDigit cs
           n = read (c:digits) :: Int
           (char:rest') = rest
       in replicate n char ++ decode rest'
   | otherwise = c : decode cs

encode :: String -> String
encode = concatMap encodeGroup . group
    where 
        encodeGroup g = 
            let n = length g
                c = head g
            in if n == 1 
               then [c] 
               else show n ++ [c]
