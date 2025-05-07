module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    if all isValid xs
        then Right $ foldr countNuc initialMap xs
        else Left "Invalid DNA sequence"
  where
    initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    isValid c = c `elem` "ACGT"
    charToNuc 'A' = A
    charToNuc 'C' = C
    charToNuc 'G' = G
    charToNuc 'T' = T
    charToNuc _   = error "Invalid nucleotide"
    countNuc c m = Map.adjust (+1) (charToNuc c) m
