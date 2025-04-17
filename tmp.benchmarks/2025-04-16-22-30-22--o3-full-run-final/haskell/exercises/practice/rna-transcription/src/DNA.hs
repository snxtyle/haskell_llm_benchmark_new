module DNA (toRNA) where

-- | Transcribes a DNA strand into its RNA complement.
--   Returns the first invalid character encountered wrapped in 'Left',
--   otherwise returns the full transcribed RNA strand wrapped in 'Right'.
toRNA :: String -> Either Char String
toRNA = mapM transcribe
  where
    transcribe :: Char -> Either Char Char
    transcribe 'G' = Right 'C'
    transcribe 'C' = Right 'G'
    transcribe 'T' = Right 'A'
    transcribe 'A' = Right 'U'
    transcribe c   = Left c
