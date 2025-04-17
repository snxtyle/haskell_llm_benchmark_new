module ProteinTranslation
  ( proteins
  ) where

-- | Translate an RNA string into its corresponding list of proteins.
--   Returns `Nothing` if the input contains an invalid codon or the length
--   is not a multiple of three.
--
--   Stops translation (and discards the stop codon and everything after it)
--   when one of the stop codons is encountered.
proteins :: String -> Maybe [String]
proteins = go []
  where
    go acc [] = Just (reverse acc)
    go _  xs | length xs < 3 = Nothing  -- incomplete trailing nucleotides
    go acc xs =
      let (codon, rest) = splitAt 3 xs
      in case translate codon of
           Invalid     -> Nothing
           Stop        -> Just (reverse acc)
           Protein p   -> go (p : acc) rest

-- --------------------------------------------------------------------
-- Internal helpers
-- --------------------------------------------------------------------

-- | Result of translating a single codon.
data Translation
  = Protein String  -- ^ A regular protein
  | Stop            -- ^ A stop codon
  | Invalid         -- ^ Unknown / invalid codon
  deriving (Eq, Show)

-- | Translate a 3‑letter codon into a protein / stop / invalid marker.
translate :: String -> Translation
translate codon = case codon of
  "AUG" -> Protein "Methionine"

  "UUU" -> Protein "Phenylalanine"
  "UUC" -> Protein "Phenylalanine"

  "UUA" -> Protein "Leucine"
  "UUG" -> Protein "Leucine"

  "UCU" -> Protein "Serine"
  "UCC" -> Protein "Serine"
  "UCA" -> Protein "Serine"
  "UCG" -> Protein "Serine"

  "UAU" -> Protein "Tyrosine"
  "UAC" -> Protein "Tyrosine"

  "UGU" -> Protein "Cysteine"
  "UGC" -> Protein "Cysteine"

  "UGG" -> Protein "Tryptophan"

  "UAA" -> Stop
  "UAG" -> Stop
  "UGA" -> Stop

  _     -> Invalid
