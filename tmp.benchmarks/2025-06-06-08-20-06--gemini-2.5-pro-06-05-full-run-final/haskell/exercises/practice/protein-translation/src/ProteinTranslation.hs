module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = fmap (takeWhile (/= "STOP")) . sequence . map translateCodon $ codons rna
  where
    codons :: String -> [String]
    codons (c1:c2:c3:cs) = [c1,c2,c3] : codons cs
    codons _             = []

    translateCodon :: String -> Maybe String
    translateCodon codon
      | codon == "AUG"                            = Just "Methionine"
      | codon `elem` ["UUU", "UUC"]               = Just "Phenylalanine"
      | codon `elem` ["UUA", "UUG"]               = Just "Leucine"
      | codon `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
      | codon `elem` ["UAU", "UAC"]               = Just "Tyrosine"
      | codon `elem` ["UGU", "UGC"]               = Just "Cysteine"
      | codon == "UGG"                            = Just "Tryptophan"
      | codon `elem` ["UAA", "UAG", "UGA"]        = Just "STOP"
      | otherwise                                 = Nothing
