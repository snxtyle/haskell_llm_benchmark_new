module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, insertWith)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- | Counts the occurrences of each nucleotide in a DNA strand.
--
-- Returns an error ('Left' String) if the strand contains invalid characters.
-- Otherwise, returns a map ('Right' (Map Nucleotide Int)) with counts for A, C, G, T.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  -- Initialize the map with all nucleotides having a count of 0.
  let initialMap :: Map Nucleotide Int
      initialMap = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

      -- Function to update the counts map for a single character.
      -- Returns Left with an error message if the character is invalid.
      updateCounts :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
      updateCounts counts char =
        case char of
          'A' -> Right $ insertWith (+) A 1 counts
          'C' -> Right $ insertWith (+) C 1 counts
          'G' -> Right $ insertWith (+) G 1 counts
          'T' -> Right $ insertWith (+) T 1 counts
          _   -> Left $ "Invalid nucleotide '" ++ [char] ++ "' found in strand."

  -- Fold over the input string, applying updateCounts.
  -- foldM handles the Either context, short-circuiting on Left.
  in foldM updateCounts initialMap xs
