module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" $ map verse [99, 98..0]

verse :: Int -> String
verse n = firstLine n ++ "\n" ++ secondLine n

firstLine :: Int -> String
firstLine n = bottles n ++ " on the wall, " ++ bottles n ++ "."

secondLine :: Int -> String
secondLine 0 = "Go to the store and buy some more, " ++ bottles 99 ++ " on the wall."
secondLine n = takeAction n ++ ", " ++ bottles (n - 1) ++ " on the wall."

takeAction :: Int -> String
takeAction 1 = "Take it down and pass it around"
takeAction _ = "Take one down and pass it around"

bottles :: Int -> String
bottles 0 = "no more bottles of beer"
bottles 1 = "1 bottle of beer"
bottles n = show n ++ " bottles of beer"
