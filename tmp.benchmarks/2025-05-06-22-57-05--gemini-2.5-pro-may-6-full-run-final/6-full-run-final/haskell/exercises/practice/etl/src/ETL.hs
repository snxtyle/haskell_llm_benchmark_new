module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower)

-- | Transforms the legacy scrabble score system to the new system.
-- The legacy system stores scores as keys and a string of uppercase letters
-- that have that score as values.
-- The new system stores individual lowercase letters as keys and their score as values.
--
-- Example:
-- transform (Map.fromList [(1, "AEIOU")])
-- returns Map.fromList [('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1)]
transform :: Map a String -> Map Char a
transform legacyData = Map.fromList newFormatData
  where
    -- Convert the legacy map to a list of (score, lettersString) pairs
    -- The explicit type signature for legacyList has been removed.
    -- Its type will be inferred as [(a, String)], where 'a' is the
    -- same type variable from the 'transform' function's signature.
    legacyList = Map.toList legacyData

    -- For each (score, lettersString) pair, create a list of (lowercaseLetter, score) pairs
    newFormatData :: [(Char, a)]
    newFormatData = concatMap processEntry legacyList

    -- Helper function to process a single (score, lettersString) entry
    processEntry :: (a, String) -> [(Char, a)]
    processEntry (score, lettersString) = map (\letter -> (toLower letter, score)) lettersString
