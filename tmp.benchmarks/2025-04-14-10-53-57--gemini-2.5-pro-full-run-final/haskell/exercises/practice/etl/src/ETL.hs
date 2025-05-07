module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Ord a => Map a String -> Map Char a
transform legacyData = Map.fromList $ concatMap processEntry $ Map.toList legacyData
  where
    processEntry :: (a, String) -> [(Char, a)]
    processEntry (score, letters) = map (\letter -> (toLower letter, score)) letters
