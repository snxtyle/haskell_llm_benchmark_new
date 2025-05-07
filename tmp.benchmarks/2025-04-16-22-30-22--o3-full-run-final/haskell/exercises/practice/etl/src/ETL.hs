module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Convert a “score ⇒ letters” map, where the letters are supplied as a single
--   upper‑case string, into a “letter ⇒ score” map with lower‑case keys.
--
--   Example input:
--
--   > fromList [(1, "AEIOULNRST"), (2, "DG")]
--
--   produces:
--
--   > fromList [('a',1), ('e',1), … , ('d',2), ('g',2)]
--
transform :: Map a String -> Map Char a
transform legacyData =
  Map.fromList
    [ (toLower letter, score)
    | (score, letters) <- Map.toList legacyData
    , letter          <- letters
    ]
