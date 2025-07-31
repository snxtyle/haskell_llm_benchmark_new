module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M

transform :: Map a String -> Map Char a
transform legacyData =
  M.fromList
    [ (toLower c, score)
    | (score, letters) <- M.toList legacyData
    , c <- letters
    ]
