module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna = 
    let chunksOf _ [] = []
        chunksOf n xs = take n xs : chunksOf n (drop n xs)
        codons = chunksOf 3 rna
        stopCodons = ["UAA", "UAG", "UGA"]
        aminoAcidMap = 
            [ ("AUG", "Methionine")
            , ("UUU", "Phenylalanine")
            , ("UUC", "Phenylalanine")
            , ("UUA", "Leucine")
            , ("UUG", "Leucine")
            , ("UCU", "Serine")
            , ("UCC", "Serine")
            , ("UCA", "Serine")
            , ("UCG", "Serine")
            , ("UAU", "Tyrosine")
            , ("UAC", "Tyrosine")
            , ("UGU", "Cysteine")
            , ("UGC", "Cysteine")
            , ("UGG", "Tryptophan")
            ]
        go [] acc = Just (reverse acc)
        go (c:cs) acc
            | length c /= 3 = Nothing
            | c `elem` stopCodons = Just (reverse acc)
            | otherwise = 
                case lookup c aminoAcidMap of
                    Just p -> go cs (p:acc)
                    Nothing -> Nothing
    in go codons []
