module Beer (song) where

import Data.Char (toUpper)
import Data.List (intercalate)

-- | Full lyrics for “99 Bottles of Beer”.
--   Constructed from individual verses to avoid the huge,
--   unreadable string literal that was here before.
song :: String
song = intercalate "\n" (map verse [99,98..0])

-- Internal helpers -----------------------------------------------------------

verse :: Int -> String
verse n =
  firstLine n ++ "\n" ++ secondLine n ++ "\n"

firstLine :: Int -> String
firstLine n =
  bottlesCapital n ++ " of beer on the wall, " ++ bottles n ++ " of beer."

secondLine :: Int -> String
secondLine 0 =
  "Go to the store and buy some more, 99 bottles of beer on the wall."
secondLine 1 =
  "Take it down and pass it around, no more bottles of beer on the wall."
secondLine 2 =
  "Take one down and pass it around, 1 bottle of beer on the wall."
secondLine n =
  "Take one down and pass it around, "
    ++ show (n - 1)
    ++ " bottles of beer on the wall."

-- Bottle phrasing ------------------------------------------------------------

bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"

bottlesCapital :: Int -> String
bottlesCapital n =
  case bottles n of
    []     -> []
    (c:cs) -> toUpper c : cs
