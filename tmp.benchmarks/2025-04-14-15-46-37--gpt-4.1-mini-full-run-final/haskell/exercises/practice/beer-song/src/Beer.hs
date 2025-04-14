module Beer (song) where

import Data.List (intercalate)
import Data.Char (toUpper)

song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

bottle :: Int -> String
bottle 0 = "no more bottles"
bottle 1 = "1 bottle"
bottle n = show n ++ " bottles"

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

verse :: Int -> String
verse n
  | n > 1 = bottleN ++ " of beer on the wall, " ++ bottleN ++ " of beer.\n" ++
            "Take one down and pass it around, " ++ bottle (n-1) ++ " of beer on the wall."
  | n == 1 = bottleN ++ " of beer on the wall, " ++ bottleN ++ " of beer.\n" ++
             "Take it down and pass it around, no more bottles of beer on the wall."
  | otherwise = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
                "Go to the store and buy some more, 99 bottles of beer on the wall."
  where bottleN = capitalize (bottle n)
