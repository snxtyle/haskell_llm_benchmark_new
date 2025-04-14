module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, empty, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | all (`elem` "ACGT") xs = Right $ foldl addNucleotide empty xs
    | otherwise              = Left "Invalid nucleotide in strand"
  where
    addNucleotide m c = case c of
        'A' -> insertWith (+) A 1 m
        'C' -> insertWith (+) C 1 m
        'G' -> insertWith (+) G 1 m
        'T' -> insertWith (+) T 1 m
