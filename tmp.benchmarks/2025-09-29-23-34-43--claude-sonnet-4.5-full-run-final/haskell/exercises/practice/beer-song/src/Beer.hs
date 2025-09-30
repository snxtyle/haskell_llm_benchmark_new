module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [99, 98..0]) ++ "\n"

verse :: Int -> String
verse n = firstLine n ++ "\n" ++ secondLine n

firstLine :: Int -> String
firstLine n = bottleCount n True ++ " of beer on the wall, " ++ bottleCount n False ++ " of beer."

secondLine :: Int -> String
secondLine 0 = "Go to the store and buy some more, 99 bottles of beer on the wall."
secondLine 1 = "Take it down and pass it around, " ++ bottleCount 0 False ++ " of beer on the wall."
secondLine n = "Take one down and pass it around, " ++ bottleCount (n - 1) False ++ " of beer on the wall."

bottleCount :: Int -> Bool -> String
bottleCount 0 True = "No more bottles"
bottleCount 0 False = "no more bottles"
bottleCount 1 _ = "1 bottle"
bottleCount n _ = show n ++ " bottles"
