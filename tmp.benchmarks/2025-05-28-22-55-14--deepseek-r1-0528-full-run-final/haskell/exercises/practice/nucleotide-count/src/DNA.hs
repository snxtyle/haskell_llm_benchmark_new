module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM step initial xs
  where
    initial = M.fromList [(A,0), (C,0), (G,0), (T,0)]
    step m c = case c of
      'A' -> Right $ M.adjust (+1) A m
      'C' -> Right $ M.adjust (+1) C m
      'G' -> Right $ M.adjust (+1) G m
      'T' -> Right $ M.adjust (+1) T m
      _    -> Left $ "Invalid character in sequence: " ++ [c]
