module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = Map.fromList $ concatMap expandEntry (Map.toList legacyData)
  where
    expandEntry (score, letters) = [(toLower letter, score) | letter <- letters]
