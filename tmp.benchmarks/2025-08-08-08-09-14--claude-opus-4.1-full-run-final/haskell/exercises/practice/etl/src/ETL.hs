module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = Map.fromList $ concatMap transformGroup (Map.toList legacyData)
  where
    transformGroup (score, letters) = [(toLower letter, score) | letter <- letters]
