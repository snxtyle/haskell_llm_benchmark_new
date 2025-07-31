module Beer (song) where

song :: String
song = unlines $ concatMap verse [99,98..0] ++ finalVerse

verse :: Int -> [String]
verse 0 = []
verse 1 = [ "1 bottle of beer on the wall, 1 bottle of beer."
          , "Take it down and pass it around, no more bottles of beer on the wall."
          , ""
          ]
verse n = [ show n ++ " bottles of beer on the wall, " ++ show n ++ " bottles of beer."
          , "Take one down and pass it around, " ++ show (n-1) ++ " " ++ bottles (n-1) ++ " of beer on the wall."
          , ""
          ]

bottles :: Int -> String
bottles 1 = "bottle"
bottles _ = "bottles"

finalVerse :: [String]
finalVerse = [ "No more bottles of beer on the wall, no more bottles of beer."
             , "Go to the store and buy some more, 99 bottles of beer on the wall."
             ]
