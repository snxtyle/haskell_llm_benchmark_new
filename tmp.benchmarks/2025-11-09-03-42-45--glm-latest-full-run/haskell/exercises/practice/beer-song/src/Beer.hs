module Beer (song) where

import Data.List (intercalate)
import Data.Char (toUpper)

-- | Returns the appropriate phrase for a given number of bottles.
-- Handles singular ("1 bottle") and the empty case ("no more bottles").
bottlePhrase :: Int -> String
bottlePhrase 0 = "no more bottles"
bottlePhrase 1 = "1 bottle"
bottlePhrase n = show n ++ " bottles"

-- | Capitalizes the first character of a string.
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

-- | Generates a single verse of the song for a given number of bottles.
verse :: Int -> String
verse n = firstLine ++ "\n" ++ secondLine
  where
    firstLine = (capitalize $ bottlePhrase n) ++ " of beer on the wall, " ++ bottlePhrase n ++ " of beer."
    secondLine = case n of
        0 -> "Go to the store and buy some more, " ++ bottlePhrase 99 ++ " of beer on the wall."
        1 -> "Take it down and pass it around, " ++ bottlePhrase 0 ++ " of beer on the wall."
        _ -> "Take one down and pass it around, " ++ bottlePhrase (n - 1) ++ " of beer on the wall."

-- | Generates the entire "99 Bottles of Beer" song.
song :: String
song = init $ intercalate "\n\n" $ map verse [99, 98 .. 0]
