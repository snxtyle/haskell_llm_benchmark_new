module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map, fromList, toList)

transform :: (Ord a) => Map a String -> Map Char a
transform legacyData = fromList [ (toLower c, score) | (score, letters) <- toList legacyData, c <- letters ]
