module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concatMap (\(score, letters) -> map (\c -> (toLower c, score)) letters) (toList legacyData)
