module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = go xs (Map.fromList [(A,0),(C,0),(G,0),(T,0)])
  where
    go [] counts = Right counts
    go (c:cs) counts = case charToNucleotide c of
      Just n  -> go cs (Map.insertWith (+) n 1 counts)
      Nothing -> Left ("Invalid nucleotide: " ++ [c])

    charToNucleotide :: Char -> Maybe Nucleotide
    charToNucleotide 'A' = Just A
    charToNucleotide 'C' = Just C
    charToNucleotide 'G' = Just G
    charToNucleotide 'T' = Just T
    charToNucleotide _   = Nothing
