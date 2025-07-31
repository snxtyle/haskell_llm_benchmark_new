module Beer (song) where

import Data.List (intercalate)
import Data.Char (toUpper)

song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

verse :: Int -> String
verse n =
  case n of
    0 ->
      "No more bottles of beer on the wall, no more bottles of beer.\n"
      ++ "Go to the store and buy some more, 99 bottles of beer on the wall."
    1 ->
      line n ++ "\n"
      ++ "Take it down and pass it around, no more bottles of beer on the wall."
    _ ->
      line n ++ "\n"
      ++ "Take one down and pass it around, " ++ bottles (n - 1) ++ " of beer on the wall."

line :: Int -> String
line n = capitalize (bottles n) ++ " of beer on the wall, " ++ bottles n ++ " of beer."

bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"

capitalize :: String -> String
capitalize []     = []
capitalize (c:cs) = toUpper c : cs
