module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse transcribe
  where
    transcribe nucleotide = case nucleotide of
        'G' -> Right 'C'
        'C' -> Right 'G'
        'T' -> Right 'A'
        'A' -> Right 'U'
        c   -> Left c
