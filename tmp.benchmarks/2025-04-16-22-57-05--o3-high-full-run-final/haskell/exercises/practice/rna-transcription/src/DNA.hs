module DNA (toRNA) where

-- | Convert a DNA strand to its RNA complement.
--   Returns 'Left' with the first invalid nucleotide, otherwise
--   'Right' with the fully transcribed RNA strand.
toRNA :: String -> Either Char String
toRNA = traverse complement
  where
    complement 'G' = Right 'C'
    complement 'C' = Right 'G'
    complement 'T' = Right 'A'
    complement 'A' = Right 'U'
    complement c   = Left c
