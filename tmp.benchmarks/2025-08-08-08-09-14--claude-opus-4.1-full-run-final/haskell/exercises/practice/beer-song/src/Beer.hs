module Beer (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" $ map verse [99, 98..0]

verse :: Int -> String
verse n = firstLine n ++ "\n" ++ secondLine n

firstLine :: Int -> String
firstLine n = capitalize (bottlesText n) ++ " on the wall, " ++ bottlesText n ++ "."

secondLine :: Int -> String
secondLine 0 = "Go to the store and buy some more, " ++ bottlesText 99 ++ " on the wall."
secondLine n = takeAction n ++ ", " ++ bottlesText (n - 1) ++ " on the wall."

takeAction :: Int -> String
takeAction 1 = "Take it down and pass it around"
takeAction _ = "Take one down and pass it around"

bottlesText :: Int -> String
bottlesText 0 = "no more bottles of beer"
bottlesText 1 = "1 bottle of beer"
bottlesText n = show n ++ " bottles of beer"

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c
