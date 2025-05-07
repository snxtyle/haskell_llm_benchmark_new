module ProteinTranslation(proteins) where

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)

proteins :: String -> Maybe [String]
proteins rna = sequence $ takeWhile (/= Just "STOP") $ map codonToProtein (codons rna)

codons :: String -> [String]
codons [] = []
codons (a:b:c:rest) = [a,b,c] : codons rest
codons _ = []  -- incomplete codon ignored

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
codonToProtein "UAA" = Just "STOP"
codonToProtein "UAG" = Just "STOP"
codonToProtein "UGA" = Just "STOP"
codonToProtein _     = Nothing
