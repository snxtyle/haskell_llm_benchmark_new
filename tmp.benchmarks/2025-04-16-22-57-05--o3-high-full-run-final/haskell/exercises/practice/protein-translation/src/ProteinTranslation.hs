module ProteinTranslation (proteins) where

-- | Translate an RNA strand into a list of proteins.
--   Returns Nothing when:
--     * The input length is not an exact multiple of three, or
--     * The input contains an invalid/unknown codon.
--   Translation stops when a STOP codon is encountered.
proteins :: String -> Maybe [String]
proteins rna
  | length rna `mod` 3 /= 0 = Nothing
  | otherwise               = translate (chunksOf3 rna)
  where
    -- Break the strand into successive triplets
    chunksOf3 :: String -> [String]
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)

    -- Recursive translation respecting STOP codons and invalid codons
    translate :: [String] -> Maybe [String]
    translate []       = Just []
    translate (c:cs) = case c of
      "AUG" -> prepend "Methionine"
      "UUU" -> prepend "Phenylalanine"
      "UUC" -> prepend "Phenylalanine"
      "UUA" -> prepend "Leucine"
      "UUG" -> prepend "Leucine"
      "UCU" -> prepend "Serine"
      "UCC" -> prepend "Serine"
      "UCA" -> prepend "Serine"
      "UCG" -> prepend "Serine"
      "UAU" -> prepend "Tyrosine"
      "UAC" -> prepend "Tyrosine"
      "UGU" -> prepend "Cysteine"
      "UGC" -> prepend "Cysteine"
      "UGG" -> prepend "Tryptophan"
      "UAA" -> Just []   -- STOP codons
      "UAG" -> Just []
      "UGA" -> Just []
      _     -> Nothing   -- Unknown codon
      where
        prepend protein = (protein :) <$> translate cs
