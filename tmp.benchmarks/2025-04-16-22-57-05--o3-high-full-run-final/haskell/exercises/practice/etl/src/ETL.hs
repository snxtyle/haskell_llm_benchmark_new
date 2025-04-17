module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Convert the legacy score representation (score → group of letters)
--   into the new representation (letter → score).
--
--   Example:
--   > transform (Map.fromList [(1, "AEIOU")])
--   fromList [('a',1),('e',1),('i',1),('o',1),('u',1)]
--
--   The function is polymorphic over the score type @a@, so it works for
--   any type that can be used as a map value (e.g. Int).
transform :: Map a String -> Map Char a
transform legacyData =
  Map.fromList
    [ (toLower letter, score)
    | (score, letters) <- Map.toList legacyData
    , letter            <- letters
    ]
