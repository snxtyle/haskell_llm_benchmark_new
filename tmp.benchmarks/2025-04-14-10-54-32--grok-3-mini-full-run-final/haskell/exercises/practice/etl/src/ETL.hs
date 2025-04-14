module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: (Ord a) => Map a String -> Map Char a
transform legacyData = fromList [ (toLower c, key) | (key, str) <- toList legacyData, c <- str ]
