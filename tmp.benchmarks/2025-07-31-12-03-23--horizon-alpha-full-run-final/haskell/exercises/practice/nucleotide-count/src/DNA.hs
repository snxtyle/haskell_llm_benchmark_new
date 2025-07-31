module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (partitionEithers)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  case partitionEithers (map charToNucleotide xs) of
    ([], ns) -> Right (foldr increment initialCounts ns)
    (err:_, _) -> Left err
  where
    initialCounts :: Map Nucleotide Int
    initialCounts = Map.fromList [(A,0),(C,0),(G,0),(T,0)]

    increment :: Nucleotide -> Map Nucleotide Int -> Map Nucleotide Int
    increment n = Map.insertWith (+) n 1

charToNucleotide :: Char -> Either String Nucleotide
charToNucleotide 'A' = Right A
charToNucleotide 'C' = Right C
charToNucleotide 'G' = Right G
charToNucleotide 'T' = Right T
charToNucleotide c   = Left $ "Invalid nucleotide: " ++ [c]
