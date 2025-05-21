module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldl folder (Right initialCounts) xs
  where
    initialCounts :: Map Nucleotide Int
    initialCounts = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    folder :: Either String (Map Nucleotide Int) -> Char -> Either String (Map Nucleotide Int)
    folder (Left err) _ = Left err -- If an error occurred previously, propagate it
    folder (Right counts) char =
      case char of
        'A' -> Right $ Map.insertWith (+) A 1 counts
        'C' -> Right $ Map.insertWith (+) C 1 counts
        'G' -> Right $ Map.insertWith (+) G 1 counts
        'T' -> Right $ Map.insertWith (+) T 1 counts
        _   -> Left "invalid nucleotide" -- Invalid character found
