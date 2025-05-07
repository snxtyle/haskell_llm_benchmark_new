module Beer (song) where

import Data.List (intercalate)

-- Helper function to get the string "N bottles" or "1 bottle".
bottleText :: Int -> String
bottleText 1 = "1 bottle"
bottleText n = show n ++ " bottles"

-- Generates a single verse of the song.
verse :: Int -> String
verse n
    | n == 0 =
        "No more bottles of beer on the wall, no more bottles of beer.\n" ++
        "Go to the store and buy some more, " ++ bottleText 99 ++ " of beer on the wall."
    | n == 1 =
        bottleText 1 ++ " of beer on the wall, " ++ bottleText 1 ++ " of beer.\n" ++
        "Take it down and pass it around, no more bottles of beer on the wall."
    | otherwise =
        bottleText n ++ " of beer on the wall, " ++ bottleText n ++ " of beer.\n" ++
        "Take one down and pass it around, " ++ bottleText (n - 1) ++ " of beer on the wall."

-- Generates the entire song.
song :: String
song = intercalate "\n\n" (map verse [99,98..0]) ++ "\n"
