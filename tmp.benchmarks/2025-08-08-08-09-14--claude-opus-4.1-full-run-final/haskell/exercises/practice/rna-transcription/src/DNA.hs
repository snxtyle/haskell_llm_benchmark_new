module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = mapM transcribeNucleotide xs
  where
    transcribeNucleotide :: Char -> Either Char Char
    transcribeNucleotide 'G' = Right 'C'
    transcribeNucleotide 'C' = Right 'G'
    transcribeNucleotide 'T' = Right 'A'
    transcribeNucleotide 'A' = Right 'U'
    transcribeNucleotide c   = Left c
