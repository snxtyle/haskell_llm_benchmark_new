module ProteinTranslation(proteins) where

isStop :: String -> Bool
isStop "UAA" = True
isStop "UAG" = True
isStop "UGA" = True
isStop _ = False

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
codonToProtein _ = Nothing

splitCodons :: String -> [String]
splitCodons [] = []
splitCodons s = take 3 s : splitCodons (drop 3 s)

translate :: [String] -> Maybe [String]
translate [] = Just []
translate (c:cs) = if isStop c then Just []
                   else case codonToProtein c of
                          Just p -> fmap (p:) (translate cs)
                          Nothing -> Nothing

proteins :: String -> Maybe [String]
proteins rna = if length rna `mod` 3 /= 0 then Nothing
               else translate (splitCodons rna)
