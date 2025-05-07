module ProteinTranslation(proteins) where

import Data.Maybe ( catMaybes, isJust )
import Data.List ( takeWhile ) -- Import takeWhile explicitly for clarity, though often in Prelude

-- Helper function to split a list into chunks of a given size.
-- It handles lists whose lengths are not multiples of n by creating a shorter chunk at the end.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Helper function to translate a single codon string into a Maybe String.
-- Returns Just "ProteinName" for a valid protein codon.
-- Returns Nothing for a STOP codon or any invalid/unrecognized/incomplete codon.
translateCodon :: String -> Maybe String
translateCodon codon = case codon of
    "AUG" -> Just "Methionine"
    "UUU" -> Just "Phenylalanine"
    "UUC" -> Just "Phenylalanine"
    "UUA" -> Just "Leucine"
    "UUG" -> Just "Leucine"
    "UCU" -> Just "Serine"
    "UCC" -> Just "Serine"
    "UCA" -> Just "Serine"
    "UCG" -> Just "Serine"
    "UAU" -> Just "Tyrosine"
    "UAC" -> Just "Tyrosine"
    "UGU" -> Just "Cysteine"
    "UGC" -> Just "Cysteine"
    "UGG" -> Just "Tryptophan"
    "UAA" -> Nothing -- STOP
    "UAG" -> Nothing -- STOP
    "UGA" -> Nothing -- STOP
    _     -> Nothing -- Treat any other 3-character sequence or incomplete sequence as STOP/invalid

-- Translates an RNA sequence string into a list of protein names.
-- Translation stops at the first STOP codon encountered.
proteins :: String -> Maybe [String]
proteins rna = Just translatedProteins
  where
    -- Split the RNA string into codons (chunks of 3 characters)
    codons = chunksOf 3 rna
    -- Translate each codon into Maybe String (Nothing for STOP/invalid)
    translatedMaybeProteins = map translateCodon codons
    -- Take the sequence of translated codons up until the first STOP (Nothing)
    validSequence = Data.List.takeWhile isJust translatedMaybeProteins
    -- Extract the protein names (String) from the Just values
    translatedProteins = catMaybes validSequence
