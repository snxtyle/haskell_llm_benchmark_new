module Beer (song) where

import Data.Char (toUpper)
import Data.List (intercalate)

-- | Full lyrics, verses separated by a blank line.
song :: String
song = intercalate "\n" $ map verse [99,98..0]

-- Build one verse -----------------------------------------------------------

verse :: Int -> String
verse n =
  case n of
    0 -> capitalize (bottles 0) ++ " on the wall, " ++ bottles 0 ++ ".\n"
         ++ "Go to the store and buy some more, " ++ bottles 99 ++ " on the wall.\n"
    _ ->
      let current = bottles n
          next    = bottles (n - 1)
          first   = capitalize current ++ " on the wall, " ++ current ++ ".\n"
          action  = if n == 1
                      then "Take it down and pass it around"
                      else "Take one down and pass it around"
          second  = action ++ ", " ++ next ++ " on the wall.\n"
      in first ++ second

-- Helpers -------------------------------------------------------------------

bottles :: Int -> String
bottles i = case i of
  0 -> "no more bottles of beer"
  1 -> "1 bottle of beer"
  _ -> show i ++ " bottles of beer"

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
