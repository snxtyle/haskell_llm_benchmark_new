module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = 
    if validInput rna
    then Just $ translateCodons $ codons rna
    else Nothing
  where
    validInput str = length str `mod` 3 == 0 && all (`elem` "AUGC") str
    codons str = [ take 3 $ drop i str | i <- [0, 3 .. length str - 3] ]
    translateCodons = takeWhile (/= "STOP") . map codonToProtein
    codonToProtein codon = case codon of
        "AUG" -> "Methionine"
        "UUU" -> "Phenylalanine"
        "UUC" -> "Phenylalanine"
        "UUA" -> "Leucine"
        "UUG" -> "Leucine"
        "UCU" -> "Serine"
        "UCC" -> "Serine"
        "UCA" -> "Serine"
        "UCG" -> "Serine"
        "UAU" -> "Tyrosine"
        "UAC" -> "Tyrosine"
        "UGU" -> "Cysteine"
        "UGC" -> "Cysteine"
        "UGG" -> "Tryptophan"
        "UAA" -> "STOP"
        "UAG" -> "STOP"
        "UGA" -> "STOP"
        _     -> "STOP" -- This case shouldn't occur due to validInput check
