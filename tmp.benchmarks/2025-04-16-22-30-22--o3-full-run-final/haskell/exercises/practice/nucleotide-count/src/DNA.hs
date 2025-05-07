module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

data Nucleotide = A | C | G | T
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Convert a character to a Nucleotide, if possible.
charToNuc :: Char -> Maybe Nucleotide
charToNuc 'A' = Just A
charToNuc 'C' = Just C
charToNuc 'G' = Just G
charToNuc 'T' = Just T
charToNuc  _  = Nothing

-- | Initial map with every nucleotide count set to 0.
initialCounts :: Map Nucleotide Int
initialCounts = Map.fromList [(n, 0) | n <- [minBound .. maxBound]]

-- | Count the occurrences of each nucleotide in a DNA strand.
--   Returns Left on the first invalid character.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts =
  foldM step initialCounts
  where
    step m c = case charToNuc c of
      Nothing -> Left "invalid nucleotide"
      Just n  -> Right $ Map.adjust (+1) n m
