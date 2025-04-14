module Beer (song) where

-- Helper function to get the correct bottle phrasing
bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"

-- Function to generate a single verse for n bottles (n from 99 down to 1)
verse :: Int -> String
verse n =
  unlines
    [ bottles n ++ " of beer on the wall, " ++ bottles n ++ " of beer.",
      action ++ " and pass it around, " ++ bottles (n - 1) ++ " of beer on the wall."
    ]
  where
    action = if n == 1 then "Take it down" else "Take one down"

-- Function for the last verse
lastVerse :: String
lastVerse =
  unlines
    [ "No more bottles of beer on the wall, no more bottles of beer.",
      "Go to the store and buy some more, 99 bottles of beer on the wall."
    ]

-- Main song function: generates the full song by combining verses
song :: String
song = unlines (map verse [99, 98 .. 1] ++ [lastVerse])
