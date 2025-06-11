module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM transcriptChar
  where
    transcriptChar :: Char -> Either Char Char
    transcriptChar 'G' = Right 'C'
    transcriptChar 'C' = Right 'G'
    transcriptChar 'T' = Right 'A'
    transcriptChar 'A' = Right 'U'
    transcriptChar nucleotide = Left nucleotide
