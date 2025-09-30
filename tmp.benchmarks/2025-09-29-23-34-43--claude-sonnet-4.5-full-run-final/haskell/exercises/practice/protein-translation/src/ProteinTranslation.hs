module ProteinTranslation(proteins) where

import Data.List (unfoldr)

proteins :: String -> Maybe [String]
proteins rna = translateCodons (splitIntoCodons rna)

-- Split RNA string into codons (groups of 3)
splitIntoCodons :: String -> [String]
splitIntoCodons = unfoldr splitThree
  where
    splitThree [] = Nothing
    splitThree xs = Just (splitAt 3 xs)

-- Translate a list of codons into proteins, stopping at STOP codons
translateCodons :: [String] -> Maybe [String]
translateCodons [] = Just []
translateCodons (codon:rest)
  | length codon /= 3 = Nothing  -- Invalid codon length
  | otherwise = case translateCodon codon of
      Nothing -> Nothing  -- Invalid codon
      Just "STOP" -> Just []  -- Stop translation
      Just protein -> fmap (protein :) (translateCodons rest)

-- Translate a single codon to its protein
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
