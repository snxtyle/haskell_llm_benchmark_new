module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = translateCodons (splitIntoCodons rna)

-- Split RNA string into codons (groups of 3)
splitIntoCodons :: String -> [String]
splitIntoCodons [] = []
splitIntoCodons rna
  | length rna < 3 = [rna]  -- Invalid codon (will be handled in translation)
  | otherwise = take 3 rna : splitIntoCodons (drop 3 rna)

-- Translate a list of codons to proteins, stopping at STOP codons
translateCodons :: [String] -> Maybe [String]
translateCodons [] = Just []
translateCodons (codon:rest) = 
  case translateCodon codon of
    Nothing -> Nothing  -- Invalid codon
    Just "STOP" -> Just []  -- Stop translation
    Just protein -> case translateCodons rest of
      Nothing -> Nothing
      Just proteins -> Just (protein : proteins)

-- Translate a single codon to its corresponding protein
translateCodon :: String -> Maybe String
translateCodon "AUG" = Just "Methionine"
translateCodon "UUU" = Just "Phenylalanine"
translateCodon "UUC" = Just "Phenylalanine"
translateCodon "UUA" = Just "Leucine"
translateCodon "UUG" = Just "Leucine"
translateCodon "UCU" = Just "Serine"
translateCodon "UCC" = Just "Serine"
translateCodon "UCA" = Just "Serine"
translateCodon "UCG" = Just "Serine"
translateCodon "UAU" = Just "Tyrosine"
translateCodon "UAC" = Just "Tyrosine"
translateCodon "UGU" = Just "Cysteine"
translateCodon "UGC" = Just "Cysteine"
translateCodon "UGG" = Just "Tryptophan"
translateCodon "UAA" = Just "STOP"
translateCodon "UAG" = Just "STOP"
translateCodon "UGA" = Just "STOP"
translateCodon _ = Nothing  -- Invalid codon
