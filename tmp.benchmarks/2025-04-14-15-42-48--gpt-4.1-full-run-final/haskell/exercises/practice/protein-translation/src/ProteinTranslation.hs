module ProteinTranslation(proteins) where

import Data.List (unfoldr)
import Data.Maybe (mapMaybe)

-- Map a codon to its corresponding protein, or Nothing for STOP/invalid
codonToProtein :: String -> Maybe (Maybe String)
codonToProtein "AUG" = Just (Just "Methionine")
codonToProtein "UUU" = Just (Just "Phenylalanine")
codonToProtein "UUC" = Just (Just "Phenylalanine")
codonToProtein "UUA" = Just (Just "Leucine")
codonToProtein "UUG" = Just (Just "Leucine")
codonToProtein "UCU" = Just (Just "Serine")
codonToProtein "UCC" = Just (Just "Serine")
codonToProtein "UCA" = Just (Just "Serine")
codonToProtein "UCG" = Just (Just "Serine")
codonToProtein "UAU" = Just (Just "Tyrosine")
codonToProtein "UAC" = Just (Just "Tyrosine")
codonToProtein "UGU" = Just (Just "Cysteine")
codonToProtein "UGC" = Just (Just "Cysteine")
codonToProtein "UGG" = Just (Just "Tryptophan")
codonToProtein "UAA" = Just Nothing -- STOP
codonToProtein "UAG" = Just Nothing -- STOP
codonToProtein "UGA" = Just Nothing -- STOP
codonToProtein _     = Nothing      -- Invalid codon

-- Split the RNA string into codons (triplets)
toCodons :: String -> [String]
toCodons [] = []
toCodons xs
  | length xs < 3 = []
  | otherwise = take 3 xs : toCodons (drop 3 xs)

proteins :: String -> Maybe [String]
proteins rna = go (toCodons rna)
  where
    go [] = Just []
    go (c:cs) =
      case codonToProtein c of
        Nothing -> Nothing -- Invalid codon
        Just Nothing -> Just [] -- STOP codon
        Just (Just p) ->
          case go cs of
            Nothing -> Nothing
            Just ps -> Just (p:ps)
