module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = translate [] rna
  where
    translate acc [] = Just (reverse acc)
    translate acc rnaSeq = 
      case lookupCodon (take 3 rnaSeq) of
        Just "STOP" -> Just (reverse acc)
        Just protein -> translate (protein : acc) (drop 3 rnaSeq)
        Nothing -> Nothing

lookupCodon :: String -> Maybe String
lookupCodon codon = case codon of
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
  _ -> Nothing
