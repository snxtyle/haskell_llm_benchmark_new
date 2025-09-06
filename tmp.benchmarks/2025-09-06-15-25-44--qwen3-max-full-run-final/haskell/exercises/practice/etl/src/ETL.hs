module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.List (concatMap)

transform :: Map Int String -> Map Char Int
transform legacyData = Map.fromList $ concatMap expandGroup (Map.toList legacyData)
  where
    expandGroup (score, letters) = [(toLower letter, score) | letter <- letters]
