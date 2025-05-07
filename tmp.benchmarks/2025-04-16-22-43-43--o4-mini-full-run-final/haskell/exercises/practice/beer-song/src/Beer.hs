module Beer (song) where

import Data.Char (toUpper)
import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

verse :: Int -> String
verse 0 =
  "No more bottles of beer on the wall, no more bottles of beer.\n" ++
  "Go to the store and buy some more, 99 bottles of beer on the wall."
verse n =
  firstLine n ++ "\n" ++ secondLine n

firstLine :: Int -> String
firstLine n = capitalizedCount n ++ " of beer on the wall, " ++ count n ++ " of beer."

secondLine :: Int -> String
secondLine n
  | n > 1     = "Take one down and pass it around, " ++ count (n - 1) ++ " of beer on the wall."
  | n == 1    = "Take it down and pass it around, no more bottles of beer on the wall."
  | otherwise = error "secondLine: invalid argument"

count :: Int -> String
count 0 = "no more bottles"
count 1 = "1 bottle"
count n = show n ++ " bottles"

capitalizedCount :: Int -> String
capitalizedCount 0 = "No more bottles"
capitalizedCount n = capitalize (count n)

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
