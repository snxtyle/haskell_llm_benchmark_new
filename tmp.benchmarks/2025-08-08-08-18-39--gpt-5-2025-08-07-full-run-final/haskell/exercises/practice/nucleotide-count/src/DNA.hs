module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = do
  ns <- traverse charToNucleotide xs
  let initial = initialCounts
  pure $ foldl' (\m n -> M.insertWith (+) n 1 m) initial ns

initialCounts :: Map Nucleotide Int
initialCounts = M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

charToNucleotide :: Char -> Either String Nucleotide
charToNucleotide 'A' = Right A
charToNucleotide 'C' = Right C
charToNucleotide 'G' = Right G
charToNucleotide 'T' = Right T
charToNucleotide _   = Left "invalid nucleotide"
