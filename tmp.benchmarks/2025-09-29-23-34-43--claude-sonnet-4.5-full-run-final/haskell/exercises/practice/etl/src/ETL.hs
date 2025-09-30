module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = Map.fromList letterScorePairs
  where
    letterScorePairs = concatMap expandEntry (Map.toList legacyData)
    expandEntry (score, letters) = [(toLower letter, score) | letter <- letters]
