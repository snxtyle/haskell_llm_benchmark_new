module ProteinTranslation(proteins) where

import Data.Maybe (mapMaybe)

proteins :: String -> Maybe [String]
proteins rna =
  case codons rna of
    Nothing     -> Nothing
    Just cs     -> Just (translateUntilStop cs)

-- Split an RNA string into codons (triplets). If the length is not a multiple
-- of 3, return Nothing.
codons :: String -> Maybe [String]
codons [] = Just []
codons s
  | length s < 3 = Nothing
  | otherwise =
      let (c, rest) = splitAt 3 s
       in case codons rest of
            Nothing -> Nothing
            Just cs -> Just (c : cs)

-- Translate codons to proteins, stopping at STOP
translateUntilStop :: [String] -> [String]
translateUntilStop [] = []
translateUntilStop (c:cs) =
  case translate c of
    Nothing      -> []          -- treat unknown codon as invalid; handled earlier by Maybe
    Just Stop    -> []          -- stop translation
    Just (Amino p) -> p : translateUntilStop cs

-- Internal representation of translation result for a codon
data CodonResult = Stop | Amino String

-- Translate a single codon; Nothing indicates an invalid/unknown codon
translate :: String -> Maybe CodonResult
translate "AUG" = Just $ Amino "Methionine"
translate "UUU" = Just $ Amino "Phenylalanine"
translate "UUC" = Just $ Amino "Phenylalanine"
translate "UUA" = Just $ Amino "Leucine"
translate "UUG" = Just $ Amino "Leucine"
translate "UCU" = Just $ Amino "Serine"
translate "UCC" = Just $ Amino "Serine"
translate "UCA" = Just $ Amino "Serine"
translate "UCG" = Just $ Amino "Serine"
translate "UAU" = Just $ Amino "Tyrosine"
translate "UAC" = Just $ Amino "Tyrosine"
translate "UGU" = Just $ Amino "Cysteine"
translate "UGC" = Just $ Amino "Cysteine"
translate "UGG" = Just $ Amino "Tryptophan"
translate "UAA" = Just Stop
translate "UAG" = Just Stop
translate "UGA" = Just Stop
translate _     = Nothing
