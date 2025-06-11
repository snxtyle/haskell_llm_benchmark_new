module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)
import Data.List (nub)

-- | Determine whether a string is an isogram.
--   An isogram is a word or phrase without a repeating letter,
--   ignoring case and allowing hyphens or spaces to appear multiple times.
--
--   Examples:
--     isIsogram "lumberjacks"   == True
--     isIsogram "background"    == True
--     isIsogram "downstream"    == True
--     isIsogram "six-year-old"  == True
--     isIsogram "isograms"      == False
isIsogram :: String -> Bool
isIsogram str =
  let letters = [toLower c | c <- str, isAlpha c]
  in length letters == length (nub letters)
