module DNA (toRNA) where

-- | Convert a DNA strand to its RNA complement.
--   If the input contains an invalid nucleotide, return that character
--   wrapped in 'Left'. Otherwise return the transcribed RNA strand in 'Right'.
--
--   Valid DNA → RNA mappings:
--     G → C
--     C → G
--     T → A
--     A → U
--
--   Examples:
--   >>> toRNA "GCTA"
--   Right "CGAU"
--
--   >>> toRNA "GCXTA"
--   Left 'X'
toRNA :: String -> Either Char String
toRNA = mapM transcribe
  where
    transcribe :: Char -> Either Char Char
    transcribe 'G' = Right 'C'
    transcribe 'C' = Right 'G'
    transcribe 'T' = Right 'A'
    transcribe 'A' = Right 'U'
    transcribe c   = Left c
