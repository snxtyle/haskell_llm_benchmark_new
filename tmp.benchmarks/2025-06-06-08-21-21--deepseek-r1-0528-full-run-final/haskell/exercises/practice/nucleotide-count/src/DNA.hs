module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, insertWith)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts s = 
     let baseMap = fromList [(A,0), (C,0), (G,0), (T,0)]
     in foldM (\m c -> do
                 nuc <- charToNucleotide c
                 return $ insertWith (+) nuc 1 m
              ) baseMap s

charToNucleotide :: Char -> Either String Nucleotide
charToNucleotide 'A' = Right A
charToNucleotide 'C' = Right C
charToNucleotide 'G' = Right G
charToNucleotide 'T' = Right T
charToNucleotide c    = Left $ "invalid nucleotide: " ++ [c]
