module ProteinTranslation(proteins) where

-- Helper function to translate a single codon string to its corresponding protein name,
-- a "STOP" signal, or Nothing if the codon is not recognized.
codonToProtein :: String -> Maybe String
codonToProtein "AUG" = Just "Methionine"
codonToProtein "UUU" = Just "Phenylalanine"
codonToProtein "UUC" = Just "Phenylalanine"
codonToProtein "UUA" = Just "Leucine"
codonToProtein "UUG" = Just "Leucine"
codonToProtein "UCU" = Just "Serine"
codonToProtein "UCC" = Just "Serine"
codonToProtein "UCA" = Just "Serine"
codonToProtein "UCG" = Just "Serine"
codonToProtein "UAU" = Just "Tyrosine"
codonToProtein "UAC" = Just "Tyrosine"
codonToProtein "UGU" = Just "Cysteine"
codonToProtein "UGC" = Just "Cysteine"
codonToProtein "UGG" = Just "Tryptophan"
codonToProtein "UAA" = Just "STOP"  -- Special marker for STOP codons
codonToProtein "UAG" = Just "STOP"
codonToProtein "UGA" = Just "STOP"
codonToProtein _     = Nothing      -- Unrecognized codon

-- Helper function to split an RNA string into a list of 3-character codons.
-- Ignores any trailing characters if they don't form a full codon.
rnaToCodons :: String -> [String]
rnaToCodons rna
    | length rna < 3 = []
    | otherwise      = take 3 rna : rnaToCodons (drop 3 rna)

-- Translates an RNA sequence into a list of proteins.
-- Returns Nothing if any codon in the sequence is invalid.
-- Stops translation upon encountering a STOP codon.
proteins :: String -> Maybe [String]
proteins rna = process (rnaToCodons rna)
  where
    process :: [String] -> Maybe [String]
    process [] = Just [] -- Base case: no more codons, return empty list of proteins
    process (currentCodon : remainingCodons) =
        case codonToProtein currentCodon of
            Just "STOP"    -> Just []  -- STOP codon encountered, terminate translation
            Just protein   ->
                -- Recursively process remaining codons and prepend current protein
                case process remainingCodons of
                    Just furtherProteins -> Just (protein : furtherProteins)
                    Nothing              -> Nothing -- Invalid codon found in later part
            Nothing        -> Nothing  -- Invalid codon encountered
