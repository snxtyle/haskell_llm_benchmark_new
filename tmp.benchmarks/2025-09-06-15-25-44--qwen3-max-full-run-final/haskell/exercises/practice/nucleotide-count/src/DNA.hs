module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = case validateSequence xs of
    Left err -> Left err
    Right validNucleotides -> Right $ countNucleotides validNucleotides
  where
    validateSequence :: String -> Either String [Nucleotide]
    validateSequence [] = Right []
    validateSequence (x:xs) = case charToNucleotide x of
        Nothing -> Left $ "invalid nucleotide " ++ [x]
        Just n -> case validateSequence xs of
            Left err -> Left err
            Right rest -> Right (n : rest)
    
    charToNucleotide :: Char -> Maybe Nucleotide
    charToNucleotide 'A' = Just A
    charToNucleotide 'C' = Just C
    charToNucleotide 'G' = Just G
    charToNucleotide 'T' = Just T
    charToNucleotide _ = Nothing
    
    countNucleotides :: [Nucleotide] -> Map Nucleotide Int
    countNucleotides ns = Map.fromListWith (+) [(n, 1) | n <- ns]
