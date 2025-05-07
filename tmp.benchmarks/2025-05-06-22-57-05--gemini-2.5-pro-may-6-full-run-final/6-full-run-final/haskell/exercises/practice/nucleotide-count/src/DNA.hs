module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)
import Data.Traversable (traverse)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  case traverse charToNucleotide xs of
    Left errMsg -> Left errMsg
    Right nucleotides -> Right (countOccurrences nucleotides)
  where
    charToNucleotide :: Char -> Either String Nucleotide
    charToNucleotide 'A' = Right A
    charToNucleotide 'C' = Right C
    charToNucleotide 'G' = Right G
    charToNucleotide 'T' = Right T
    charToNucleotide other = Left ("Invalid nucleotide: " ++ [other])

    countOccurrences :: [Nucleotide] -> Map Nucleotide Int
    countOccurrences ns = foldl updateCounts initialMap ns
      where
        initialMap :: Map Nucleotide Int
        initialMap = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

        updateCounts :: Map Nucleotide Int -> Nucleotide -> Map Nucleotide Int
        updateCounts currentMap nucleotide = adjust (+1) nucleotide currentMap
