module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
  case traverse charToNucleotide xs of
    Left err -> Left err
    Right nucleotides -> Right $ countNucleotides nucleotides

charToNucleotide :: Char -> Either String Nucleotide
charToNucleotide 'A' = Right A
charToNucleotide 'C' = Right C
charToNucleotide 'G' = Right G
charToNucleotide 'T' = Right T
charToNucleotide c   = Left $ "invalid nucleotide '" ++ [c] ++ "'"

countNucleotides :: [Nucleotide] -> Map Nucleotide Int
countNucleotides nucleotides = 
  foldr (\n -> Map.insertWith (+) n 1) initialMap nucleotides
  where
    initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
