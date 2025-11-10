module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concatMap convertPair (toList legacyData)
  where
    convertPair :: (a, String) -> [(Char, a)]
    convertPair (score, letters) = map (\letter -> (toLower letter, score)) letters
