module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM nucleotideToRNA
  where
    nucleotideToRNA :: Char -> Either Char Char
    nucleotideToRNA 'G' = Right 'C'
    nucleotideToRNA 'C' = Right 'G'
    nucleotideToRNA 'T' = Right 'A'
    nucleotideToRNA 'A' = Right 'U'
    nucleotideToRNA c   = Left c
