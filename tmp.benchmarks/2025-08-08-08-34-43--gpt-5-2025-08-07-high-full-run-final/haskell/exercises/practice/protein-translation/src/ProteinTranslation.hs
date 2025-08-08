module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = do
  codons <- chunk rna
  translate codons
  where
    -- Split the RNA string into 3-character codons.
    -- Fail with Nothing if the length is not a multiple of 3.
    chunk :: String -> Maybe [String]
    chunk [] = Just []
    chunk (a:b:c:rest) = ([a, b, c] :) <$> chunk rest
    chunk _ = Nothing

    -- Translate codons to proteins, stopping at STOP codons.
    translate :: [String] -> Maybe [String]
    translate [] = Just []
    translate (c:cs) =
      case codon c of
        Nothing -> Nothing              -- invalid/unknown codon
        Just Nothing -> Just []         -- STOP codon: terminate translation
        Just (Just p) -> (p :) <$> translate cs

    -- Map a codon to either:
    --   Just (Just protein) for a protein,
    --   Just Nothing for STOP,
    --   Nothing for invalid codon.
    codon :: String -> Maybe (Maybe String)
    codon c = case c of
      "AUG" -> j "Methionine"
      "UUU" -> j "Phenylalanine"
      "UUC" -> j "Phenylalanine"
      "UUA" -> j "Leucine"
      "UUG" -> j "Leucine"
      "UCU" -> j "Serine"
      "UCC" -> j "Serine"
      "UCA" -> j "Serine"
      "UCG" -> j "Serine"
      "UAU" -> j "Tyrosine"
      "UAC" -> j "Tyrosine"
      "UGU" -> j "Cysteine"
      "UGC" -> j "Cysteine"
      "UGG" -> j "Tryptophan"
      "UAA" -> Just Nothing
      "UAG" -> Just Nothing
      "UGA" -> Just Nothing
      _     -> Nothing
      where
        j s = Just (Just s)
