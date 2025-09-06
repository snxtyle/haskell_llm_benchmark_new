module RunLength (decode, encode) where

import Data.Char (isDigit, isAlpha, isSpace)

decode :: String -> String
decode [] = []
decode encodedText = 
    let (countStr, rest) = span isDigit encodedText
    in if null countStr
        then head rest : decode (tail rest)
        else replicate (read countStr) (head rest) ++ decode (tail rest)

encode :: String -> String
encode = encodeHelper 1
  where
    encodeHelper :: Int -> String -> String
    encodeHelper _ [] = []
    encodeHelper count [x] = showCount count ++ [x]
    encodeHelper count (x:y:xs)
        | x == y = encodeHelper (count + 1) (y:xs)
        | otherwise = showCount count ++ [x] ++ encodeHelper 1 (y:xs)
    
    showCount :: Int -> String
    showCount 1 = ""
    showCount n = show n
