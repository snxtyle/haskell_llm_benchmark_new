module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [99, 98..0]) ++ "\n"

verse :: Int -> String
verse 0 = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
          "Go to the store and buy some more, 99 bottles of beer on the wall."
verse n =
    currentBottles ++ " on the wall, " ++ currentBottles ++ ".\n" ++
    "Take " ++ pronoun ++ " down and pass it around, " ++ nextBottles ++ " on the wall."
  where
    currentBottles = bottles n
    nextBottles    = bottles (n-1)
    pronoun        = if n == 1 then "it" else "one"

bottles :: Int -> String
bottles 1 = "1 bottle of beer"
bottles 0 = "no more bottles of beer"
bottles n = show n ++ " bottles of beer"
