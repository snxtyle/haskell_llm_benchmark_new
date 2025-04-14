module DNA (toRNA) where

-- | Converts a single DNA nucleotide character to its RNA complement.
-- Returns Left with the character itself if it's not a valid DNA nucleotide.
dnaToRnaNucleotide :: Char -> Either Char Char
dnaToRnaNucleotide 'G' = Right 'C'
dnaToRnaNucleotide 'C' = Right 'G'
dnaToRnaNucleotide 'T' = Right 'A'
dnaToRnaNucleotide 'A' = Right 'U'
dnaToRnaNucleotide invalid = Left invalid

-- | Transcribes a DNA strand into its RNA complement.
-- If the input string contains any invalid DNA nucleotides,
-- returns Left with the first invalid character encountered.
-- Otherwise, returns Right with the transcribed RNA string.
toRNA :: String -> Either Char String
toRNA xs = mapM dnaToRnaNucleotide xs
