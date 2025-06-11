module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- Convert a character representation into a Nucleotide.
-- Returns Nothing for invalid characters.
charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _   = Nothing

-- | Count occurrences of each nucleotide in a DNA sequence.
--   If the sequence contains anything other than A, C, G or T
--   an error is returned.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldM updateCount initialCounts
  where
    -- All nucleotide counts start at 0.
    initialCounts :: Map Nucleotide Int
    initialCounts = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    updateCount :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
    updateCount counts ch =
      case charToNucleotide ch of
        Just n  -> Right $ Map.adjust (+1) n counts
        Nothing -> Left "invalid nucleotide"
