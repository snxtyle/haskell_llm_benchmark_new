module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM step initial xs
  where
    initial :: Map Nucleotide Int
    initial = M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    step :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
    step m ch =
      case charToNucleotide ch of
        Just n  -> Right (M.adjust (+1) n m)
        Nothing -> Left "Invalid nucleotide"

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _   = Nothing
