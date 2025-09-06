module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    if all isValidNucleotide xs
        then Right $ countNucleotides xs
        else Left "Invalid nucleotide in input"
    where
        isValidNucleotide c = c `elem` "ACGT"
        countNucleotides = foldr countChar (Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
        countChar 'A' = Map.adjust (+1) A
        countChar 'C' = Map.adjust (+1) C
        countChar 'G' = Map.adjust (+1) G
        countChar 'T' = Map.adjust (+1) T
        countChar _   = id  -- This case should never happen due to validation
