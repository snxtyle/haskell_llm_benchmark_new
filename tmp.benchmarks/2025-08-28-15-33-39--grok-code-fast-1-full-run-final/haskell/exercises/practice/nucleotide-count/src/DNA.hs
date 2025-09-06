module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

toNucleotide :: Char -> Nucleotide
toNucleotide 'A' = A
toNucleotide 'C' = C
toNucleotide 'G' = G
toNucleotide 'T' = T

isValid :: Char -> Bool
isValid c = c `elem` "ACGT"

countNucleotides :: String -> Map Nucleotide Int
countNucleotides xs = foldl (\m c -> insertWith (+) (toNucleotide c) 1 m) initialMap xs
  where initialMap = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all isValid xs = Right (countNucleotides xs)
  | otherwise = Left "Invalid nucleotide in strand"
