module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- | Count the amount of each nucleotide in the supplied DNA strand.
--   Returns an error if a character other than A, C, G or T is found.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM update initial xs
  where
    -- Starting map with a zero count for every nucleotide
    initial :: Map Nucleotide Int
    initial = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    -- Attempt to convert the current character to a Nucleotide and
    -- update the running tally.  On failure, propagate an error.
    update :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
    update acc c =
      case charToNucleotide c of
        Just n  -> Right $ Map.adjust (+ 1) n acc
        Nothing -> Left "invalid nucleotide"

    -- Helper to translate a character into a Nucleotide.
    charToNucleotide :: Char -> Maybe Nucleotide
    charToNucleotide 'A' = Just A
    charToNucleotide 'C' = Just C
    charToNucleotide 'G' = Just G
    charToNucleotide 'T' = Just T
    charToNucleotide _   = Nothing
