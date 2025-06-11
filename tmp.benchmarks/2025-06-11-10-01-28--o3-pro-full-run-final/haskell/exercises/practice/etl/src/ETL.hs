module ETL (transform) where

import Data.Char (isAlpha, toLower)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Convert a legacy score map (point value → string of uppercase letters)
--   into a map of individual lowercase letters to their corresponding score.
--
--   Example:
--     from: 1 -> "AEIOU"
--     to:   'a' -> 1, 'e' -> 1, ...
transform :: Map a String -> Map Char a
transform legacyData =
  Map.fromList
    [ (toLower letter, score)
    | (score, letters) <- Map.toList legacyData
    , letter           <- letters
    , isAlpha letter
    ]
