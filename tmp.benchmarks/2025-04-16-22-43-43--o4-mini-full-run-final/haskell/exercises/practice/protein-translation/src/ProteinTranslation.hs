module ProteinTranslation (proteins) where

proteins :: String -> Maybe [String]
proteins rna = go rna []
  where
    go [] acc = Just (reverse acc)
    go (a:b:c:rest) acc =
      case translate [a,b,c] of
        Nothing      -> Nothing
        Just "STOP"  -> Just (reverse acc)
        Just protein -> go rest (protein : acc)
    go _ _ = Nothing

translate :: String -> Maybe String
translate codon = case codon of
  "AUG" -> Just "Methionine"
  "UUU" -> Just "Phenylalanine"
  "UUC" -> Just "Phenylalanine"
  "UUA" -> Just "Leucine"
  "UUG" -> Just "Leucine"
  "UCU" -> Just "Serine"
  "UCC" -> Just "Serine"
  "UCA" -> Just "Serine"
  "UCG" -> Just "Serine"
  "UAU" -> Just "Tyrosine"
  "UAC" -> Just "Tyrosine"
  "UGU" -> Just "Cysteine"
  "UGC" -> Just "Cysteine"
  "UGG" -> Just "Tryptophan"
  "UAA" -> Just "STOP"
  "UAG" -> Just "STOP"
  "UGA" -> Just "STOP"
  _     -> Nothing
