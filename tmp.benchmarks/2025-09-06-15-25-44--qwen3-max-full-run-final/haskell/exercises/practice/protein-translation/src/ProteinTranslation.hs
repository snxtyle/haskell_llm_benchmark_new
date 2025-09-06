module ProteinTranslation(proteins) where

import Data.Maybe (mapMaybe)

proteins :: String -> Maybe [String]
proteins rna
  | length rna `mod` 3 /= 0 = Nothing
  | otherwise = Just $ takeWhile (/= "STOP") $ mapMaybe translateCodon codons
  where
    codons = chunksOf 3 rna

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs < n = []
  | otherwise = take n xs : chunksOf n (drop n xs)

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
translateCodon _ = Nothing
