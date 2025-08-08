module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

verse :: Int -> String
verse n = firstLine n ++ "\n" ++ secondLine n

firstLine :: Int -> String
firstLine n =
  bottlesCap n ++ " of beer on the wall, " ++ bottles n ++ " of beer."

secondLine :: Int -> String
secondLine n
  | n > 1     = "Take one down and pass it around, " ++ bottles (n - 1) ++ " of beer on the wall."
  | n == 1    = "Take it down and pass it around, no more bottles of beer on the wall."
  | otherwise = "Go to the store and buy some more, 99 bottles of beer on the wall."

bottles :: Int -> String
bottles n
  | n == 0    = "no more bottles"
  | n == 1    = "1 bottle"
  | otherwise = show n ++ " bottles"

bottlesCap :: Int -> String
bottlesCap 0 = "No more bottles"
bottlesCap n = bottles n
