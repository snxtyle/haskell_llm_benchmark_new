module Beer (song) where

import Data.Char (toUpper)
import Data.List (intercalate)

-- | Generates the entire "99 Bottles of Beer" song.
song :: String
song = intercalate "\n" $ map verse [99, 98 .. 0]

-- | Generates a single verse for a given number of bottles.
verse :: Int -> String
verse n = line1 n ++ "\n" ++ line2 n ++ "\n"
  where
    -- Generates the first line of a verse.
    line1 :: Int -> String
    line1 num = capitalize (bottles num) ++ " of beer on the wall, " ++ bottles num ++ " of beer."

    -- Generates the second line of a verse, handling special cases.
    line2 :: Int -> String
    line2 0   = "Go to the store and buy some more, 99 bottles of beer on the wall."
    line2 1   = "Take it down and pass it around, no more bottles of beer on the wall."
    line2 num = "Take one down and pass it around, " ++ bottles (num - 1) ++ " of beer on the wall."

-- | Returns the correct string representation for the number of bottles.
-- | Handles pluralization ("bottle" vs "bottles") and the "no more" case.
bottles :: Int -> String
bottles n
    | n < 0     = error "Negative bottles requested!" -- Should not happen with current logic
    | n == 0    = "no more bottles"
    | n == 1    = "1 bottle"
    | otherwise = show n ++ " bottles"

-- | Capitalizes the first letter of a string.
capitalize :: String -> String
capitalize []     = []
capitalize (c:cs) = toUpper c : cs
