module ProteinTranslation (proteins) where

-- | Translate an RNA strand into a list of proteins.
--   Returns Nothing when an invalid codon is encountered
--   or the strand length is not a multiple of three.
--   Translation stops (without including the STOP codon)
--   when a terminating codon is reached.
proteins :: String -> Maybe [String]
proteins rna = translateCodons rna []
  where
    translateCodons :: String -> [String] -> Maybe [String]
    translateCodons [] acc = Just (reverse acc)
    translateCodons xs acc =
      let (codon, rest) = splitAt 3 xs
       in if length codon < 3
            then Nothing                    -- incomplete trailing codon
            else
              case codonToProtein codon of
                Nothing         -> Nothing  -- invalid codon
                Just Nothing    -> Just (reverse acc)    -- STOP
                Just (Just prot) -> translateCodons rest (prot : acc)

-- | Map a codon to either:
--   * Just (Just protein) for a valid protein-coding codon
--   * Just Nothing        for a STOP codon
--   * Nothing             for an invalid/unrecognised codon
codonToProtein :: String -> Maybe (Maybe String)
codonToProtein c = case c of
  "AUG" -> mk "Methionine"
  "UUU" -> mk "Phenylalanine"
  "UUC" -> mk "Phenylalanine"
  "UUA" -> mk "Leucine"
  "UUG" -> mk "Leucine"
  "UCU" -> mk "Serine"
  "UCC" -> mk "Serine"
  "UCA" -> mk "Serine"
  "UCG" -> mk "Serine"
  "UAU" -> mk "Tyrosine"
  "UAC" -> mk "Tyrosine"
  "UGU" -> mk "Cysteine"
  "UGC" -> mk "Cysteine"
  "UGG" -> mk "Tryptophan"
  "UAA" -> Just Nothing       -- STOP codons
  "UAG" -> Just Nothing
  "UGA" -> Just Nothing
  _     -> Nothing            -- Unknown codon
  where
    mk p = Just (Just p)
