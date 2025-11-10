module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, fromListWith, unionWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- | Convert a character to a Nucleotide, returning Nothing for invalid characters.
charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _   = Nothing

-- | Count the nucleotides in a DNA strand.
-- Returns a map with counts for each nucleotide, or an error if the strand is invalid.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts strand =
  case mapM charToNucleotide strand of
    Nothing -> Left "Invalid nucleotide in strand"
    Just nucleotides ->
      let -- Count the nucleotides present in the strand
          counts = fromListWith (+) (map (\n -> (n, 1)) nucleotides)
          -- Create a map with all nucleotides initialized to 0
          zeros = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
          -- Combine the two maps, ensuring all nucleotides are in the final result
          finalMap = unionWith (+) zeros counts
      in Right finalMap
