module Beer (song) where

bottle :: Int -> String
bottle 0 = "no more bottles"
bottle 1 = "1 bottle"
bottle n = show n ++ " bottles"

verse :: Int -> String
verse n = let b = bottle n
              nextB = bottle (n - 1)
              takeW = if n == 1 then "it" else "one"
          in b ++ " of beer on the wall, " ++ b ++ " of beer.\n" ++
             "Take " ++ takeW ++ " down and pass it around, " ++ nextB ++ " of beer on the wall.\n\n"

finalVerse :: String
finalVerse = "No more bottles of beer on the wall, no more bottles of beer.\n" ++
             "Go to the store and buy some more, 99 bottles of beer on the wall.\n"

song :: String
song = concat [verse n | n <- [99,98..1]] ++ finalVerse
