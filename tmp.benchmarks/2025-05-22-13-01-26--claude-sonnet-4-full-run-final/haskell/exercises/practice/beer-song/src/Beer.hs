module Beer (song) where

song :: String
song = unlines $ init $ concatMap (\n -> [verse n, ""]) [99,98..0]

verse :: Int -> String
verse 0 = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
          "Go to the store and buy some more, 99 bottles of beer on the wall."
verse 1 = "1 bottle of beer on the wall, 1 bottle of beer.\n" ++
          "Take it down and pass it around, no more bottles of beer on the wall."
verse n = show n ++ " bottles of beer on the wall, " ++ show n ++ " bottles of beer.\n" ++
          "Take one down and pass it around, " ++ bottleCount (n-1) ++ " on the wall."

bottleCount :: Int -> String
bottleCount 0 = "no more bottles of beer"
bottleCount 1 = "1 bottle of beer"
bottleCount n = show n ++ " bottles of beer"
