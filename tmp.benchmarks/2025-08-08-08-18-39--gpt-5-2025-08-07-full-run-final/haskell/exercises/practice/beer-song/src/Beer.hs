module Beer (song) where

import Data.List (intercalate)
import Data.Char (toUpper)

song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

verse :: Int -> String
verse n
  | n == 0 =
      cap (bottles 0) ++ " of beer on the wall, " ++ bottles 0 ++ " of beer.\n" ++
      "Go to the store and buy some more, 99 bottles of beer on the wall."
  | n == 1 =
      cap (bottles 1) ++ " of beer on the wall, " ++ bottles 1 ++ " of beer.\n" ++
      "Take it down and pass it around, " ++ bottles 0 ++ " of beer on the wall."
  | otherwise =
      cap (bottles n) ++ " of beer on the wall, " ++ bottles n ++ " of beer.\n" ++
      "Take one down and pass it around, " ++ bottles (n - 1) ++ " of beer on the wall."

bottles :: Int -> String
bottles k
  | k == 0 = "no more bottles"
  | k == 1 = "1 bottle"
  | otherwise = show k ++ " bottles"

cap :: String -> String
cap [] = []
cap (c:cs) = toUpper c : cs
