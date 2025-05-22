module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna
  | length rna `mod` 3 /= 0 = Nothing
  | otherwise = translateCodons (chunksOf3 rna)
  where
    chunksOf3 [] = []
    chunksOf3 xs = take 3 xs : chunksOf3 (drop 3 xs)
    
    translateCodons [] = Just []
    translateCodons (codon:rest) =
      case translateCodon codon of
        Nothing -> Nothing
        Just "STOP" -> Just []
        Just protein -> case translateCodons rest of
          Nothing -> Nothing
          Just proteins' -> Just (protein : proteins')
    
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
    translateCodon _ = Nothing
