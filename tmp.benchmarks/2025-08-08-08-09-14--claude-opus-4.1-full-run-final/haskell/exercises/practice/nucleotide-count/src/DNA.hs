module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    case parseNucleotides xs of
        Left err -> Left err
        Right nucleotides -> Right (countNucleotides nucleotides)
  where
    -- Parse string into list of Nucleotides, returning error if invalid char found
    parseNucleotides :: String -> Either String [Nucleotide]
    parseNucleotides [] = Right []
    parseNucleotides (c:cs) = 
        case charToNucleotide c of
            Nothing -> Left ("Invalid nucleotide '" ++ [c] ++ "'")
            Just n -> case parseNucleotides cs of
                Left err -> Left err
                Right ns -> Right (n:ns)
    
    -- Convert a character to a Nucleotide
    charToNucleotide :: Char -> Maybe Nucleotide
    charToNucleotide 'A' = Just A
    charToNucleotide 'C' = Just C
    charToNucleotide 'G' = Just G
    charToNucleotide 'T' = Just T
    charToNucleotide _ = Nothing
    
    -- Count nucleotides, ensuring all four types are in the map
    countNucleotides :: [Nucleotide] -> Map Nucleotide Int
    countNucleotides ns = 
        let counts = foldr (\n acc -> Map.insertWith (+) n 1 acc) Map.empty ns
        in Map.union counts initialMap
    
    -- Initial map with all nucleotides set to 0
    initialMap :: Map Nucleotide Int
    initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
