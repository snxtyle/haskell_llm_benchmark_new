module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map Int String -> Map Char Int
transform legacyData = Map.fromList $ concatMap toPairs $ Map.toList legacyData
  where
    toPairs (score, letters) = [(toLower c, score) | c <- letters]
