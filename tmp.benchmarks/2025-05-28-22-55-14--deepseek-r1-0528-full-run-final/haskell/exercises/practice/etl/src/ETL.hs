module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = 
    Map.fromList [ (toLower letter, score) 
                | (score, letters) <- Map.toList legacyData
                , letter <- letters 
                ]
