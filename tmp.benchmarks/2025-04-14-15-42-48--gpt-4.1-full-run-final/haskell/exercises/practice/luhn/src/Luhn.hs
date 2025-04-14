module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n =
    let stripped = filter (/= ' ') n
    in  length stripped > 1
        && all isDigit stripped
        && luhnSum stripped `mod` 10 == 0

luhnSum :: String -> Int
luhnSum s = sum $ zipWith ($) (cycleFuncs (length s)) (map digitToInt $ reverse s)
  where
    cycleFuncs l = take l $ cycle [id, luhnDouble]

luhnDouble :: Int -> Int
luhnDouble d = let x = d * 2 in if x > 9 then x - 9 else x
