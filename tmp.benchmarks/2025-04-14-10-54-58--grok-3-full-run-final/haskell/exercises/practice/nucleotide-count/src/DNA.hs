module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    if all isValidNucleotide xs
        then Right $ countNucleotides xs
        else Left "Invalid nucleotide in strand"

isValidNucleotide :: Char -> Bool
isValidNucleotide c = c `elem` ['A', 'C', 'G', 'T']

countNucleotides :: String -> Map Nucleotide Int
countNucleotides xs = Map.fromListWith (+) $ map toCount xs
    where
        toCount 'A' = (A, 1)
        toCount 'C' = (C, 1)
        toCount 'G' = (G, 1)
        toCount 'T' = (T, 1)
        toCount _   = error "Invalid nucleotide" -- This should never happen due to prior validation
