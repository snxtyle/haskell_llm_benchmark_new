module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
  case mapM charToNucleotide xs of
    Left err -> Left err
    Right nucleotides -> Right $ foldr countNucleotide initialCounts nucleotides
  where
    charToNucleotide :: Char -> Either String Nucleotide
    charToNucleotide 'A' = Right A
    charToNucleotide 'C' = Right C
    charToNucleotide 'G' = Right G
    charToNucleotide 'T' = Right T
    charToNucleotide c   = Left $ "Invalid nucleotide: " ++ [c]
    
    initialCounts :: Map Nucleotide Int
    initialCounts = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    
    countNucleotide :: Nucleotide -> Map Nucleotide Int -> Map Nucleotide Int
    countNucleotide n counts = Map.adjust (+1) n counts
