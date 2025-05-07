module DNA (toRNA) where

-- Helper function to transcribe a single DNA nucleotide to RNA
-- or return Left with the invalid character.
rnaNucleotide :: Char -> Either Char Char
rnaNucleotide dnaNuc = case dnaNuc of
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  invalid -> Left invalid

-- Transcribes a DNA string to an RNA string.
-- If the input string contains any invalid DNA nucleotide,
-- it returns Left with the first invalid character.
toRNA :: String -> Either Char String
toRNA xs = mapM rnaNucleotide xs
