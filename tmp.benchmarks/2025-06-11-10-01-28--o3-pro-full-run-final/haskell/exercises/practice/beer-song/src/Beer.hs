module Beer (song) where

import Data.List (intercalate)

-- | Full lyrics for “99 Bottles of Beer”.
--
--   The verses are generated programmatically to avoid the huge,
--   repetitive string literal that used to live here.
song :: String
song =
  -- Interleave the individual verses with a blank line and make sure
  -- there is a newline at the very end of the lyrics.
  intercalate "\n\n" (map verse [99,98..0]) ++ "\n"

------------------------------------------------------------------------
-- Verse generation
------------------------------------------------------------------------

-- | Render a single verse for the given number of bottles.
verse :: Int -> String
verse n
  | n > 2     = common n (bottles (n-1)) "one"
  | n == 2    = common n (bottles 1)     "one"
  | n == 1    = specialOne
  | otherwise = specialZero
  where
    -- Verse for the usual “take one down” pattern.
    common :: Int -> String -> String -> String
    common k next howMany =
      firstLine k ++ "\n" ++
      "Take " ++ howMany ++ " down and pass it around, " ++ next ++ " of beer on the wall."

    -- Verse when only a single bottle remains.
    specialOne :: String
    specialOne =
      "1 bottle of beer on the wall, 1 bottle of beer.\n" ++
      "Take it down and pass it around, no more bottles of beer on the wall."

    -- Verse when no bottles are left.
    specialZero :: String
    specialZero =
      "No more bottles of beer on the wall, no more bottles of beer.\n" ++
      "Go to the store and buy some more, 99 bottles of beer on the wall."

    -- First line is shared by the “common” cases.
    firstLine :: Int -> String
    firstLine k = bottles k ++ " of beer on the wall, " ++ bottles k ++ " of beer."

-- | Utility to format the bottle count with correct pluralisation.
bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"
