module Beer (song) where

song :: String
song = unlines $ concatMap addEmptyLine (map verse [99,98..0])
  where
    addEmptyLine lines = lines ++ [""]

verse :: Int -> [String]
verse 0 = 
    [ "No more bottles of beer on the wall, no more bottles of beer."
    , "Go to the store and buy some more, 99 bottles of beer on the wall."
    ]
verse 1 = 
    [ "1 bottle of beer on the wall, 1 bottle of beer."
    , "Take it down and pass it around, no more bottles of beer on the wall."
    ]
verse 2 = 
    [ "2 bottles of beer on the wall, 2 bottles of beer."
    , "Take one down and pass it around, 1 bottle of beer on the wall."
    ]
verse n = 
    [ show n ++ " bottles of beer on the wall, " ++ show n ++ " bottles of beer."
    , "Take one down and pass it around, " ++ show (n-1) ++ " bottles of beer on the wall."
    ]
