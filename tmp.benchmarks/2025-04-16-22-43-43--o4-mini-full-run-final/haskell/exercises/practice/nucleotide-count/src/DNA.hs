module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  case traverse parseNucleotide xs of
    Left err  -> Left err
    Right nts -> Right $ foldr countOne initialMap nts
  where
    initialMap :: Map Nucleotide Int
    initialMap = Map.fromList [(A,0), (C,0), (G,0), (T,0)]

    countOne :: Nucleotide -> Map Nucleotide Int -> Map Nucleotide Int
    countOne n = Map.insertWith (+) n 1

parseNucleotide :: Char -> Either String Nucleotide
parseNucleotide c = case c of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _   -> Left $ "invalid nucleotide '" ++ [c] ++ "'"
