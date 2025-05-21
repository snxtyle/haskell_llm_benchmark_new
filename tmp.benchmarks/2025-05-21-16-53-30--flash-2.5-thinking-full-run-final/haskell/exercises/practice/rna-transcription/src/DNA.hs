module DNA (toRNA) where

import Data.Traversable (traverse)

-- Helper function to convert a single DNA nucleotide to an RNA nucleotide.
-- Returns Right with the RNA complement if valid, or Left with the original
-- character if it's an invalid DNA nucleotide.
dnaToRnaChar :: Char -> Either Char Char
dnaToRnaChar 'G' = Right 'C'
dnaToRnaChar 'C' = Right 'G'
dnaToRnaChar 'T' = Right 'A'
dnaToRnaChar 'A' = Right 'U'
dnaToRnaChar x    = Left x -- Handle invalid characters

-- Converts a DNA sequence string to its RNA complement.
-- Returns Right with the RNA string if all characters are valid DNA nucleotides,
-- or Left with the first invalid character encountered.
toRNA :: String -> Either Char String
toRNA = traverse dnaToRnaChar
