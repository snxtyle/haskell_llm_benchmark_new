module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
    case parseNucleotides xs of
        Left err -> Left err
        Right nucleotides -> Right (countNucleotides nucleotides)

parseNucleotides :: String -> Either String [Nucleotide]
parseNucleotides [] = Right []
parseNucleotides (x:xs) = 
    case charToNucleotide x of
        Nothing -> Left ("Invalid nucleotide '" ++ [x] ++ "'")
        Just n -> case parseNucleotides xs of
            Left err -> Left err
            Right ns -> Right (n:ns)

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _ = Nothing

countNucleotides :: [Nucleotide] -> Map Nucleotide Int
countNucleotides ns = 
    let initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    in foldr (\n acc -> Map.insertWith (+) n 1 acc) initialMap ns
