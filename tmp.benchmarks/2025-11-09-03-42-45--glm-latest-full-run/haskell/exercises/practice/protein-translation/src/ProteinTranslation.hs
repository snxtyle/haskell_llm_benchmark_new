module ProteinTranslation(proteins) where

import qualified Data.Map as Map

-- | A map from RNA codons to their corresponding protein names.
-- STOP codons are mapped to the string "STOP".
codonMap :: Map.Map String String
codonMap = Map.fromList
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
    , ("UAA", "STOP")
    , ("UAG", "STOP")
    , ("UGA", "STOP")
    ]

-- | Splits a string into chunks of a given size.
chunk :: Int -> String -> [String]
chunk _ [] = []
chunk n s = take n s : chunk n (drop n s)

-- | Translates a list of codons into a list of proteins.
-- It stops translation upon encountering a STOP codon and returns Nothing
-- for any invalid codon.
translate :: [String] -> Maybe [String]
translate [] = Just []
translate (c:cs) =
    case Map.lookup c codonMap of
        Nothing          -> Nothing -- Invalid codon
        Just "STOP"      -> Just []  -- Stop codon, terminate translation
        Just protein     -> fmap (protein :) (translate cs)

-- | Translates an RNA sequence into a list of proteins.
-- Returns Nothing if the RNA sequence contains an invalid codon
-- or has a length that is not a multiple of 3.
proteins :: String -> Maybe [String]
proteins rna = translate (chunk 3 rna)
