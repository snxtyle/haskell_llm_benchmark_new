module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = transcribe []
  where
    transcribe acc [] = Right (reverse acc)
    transcribe acc (x:xs) = 
      case transcribeNucleotide x of
        Just rna -> transcribe (rna:acc) xs
        Nothing  -> Left x
    
    transcribeNucleotide :: Char -> Maybe Char
    transcribeNucleotide 'G' = Just 'C'
    transcribeNucleotide 'C' = Just 'G'
    transcribeNucleotide 'T' = Just 'A'
    transcribeNucleotide 'A' = Just 'U'
    transcribeNucleotide _   = Nothing
