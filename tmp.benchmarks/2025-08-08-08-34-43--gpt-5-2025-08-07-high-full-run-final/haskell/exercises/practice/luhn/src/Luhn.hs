module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid input =
  let stripped = filter (/= ' ') input
  in if length stripped <= 1
        then False
        else if any (not . isDigit) stripped
               then False
               else luhn stripped
  where
    luhn :: String -> Bool
    luhn s =
      let digits = map digitToInt (reverse s)
          adjusted = zipWith adjust [0..] digits
      in sum adjusted `mod` 10 == 0

    adjust :: Int -> Int -> Int
    adjust idx d
      | odd idx   = let x = d * 2 in if x > 9 then x - 9 else x
      | otherwise = d
