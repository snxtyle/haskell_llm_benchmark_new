module ProteinTranslation(proteins) where

import Data.Map (Map, fromList, lookup)
import Data.List (unfoldr)

-- | Maps codons to their corresponding protein names.
codonMap :: Map String String
codonMap = fromList
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

-- | List of codons that signal the end of translation.
stopCodons :: [String]
stopCodons = ["UAA", "UAG", "UGA"]

-- | Splits a list into chunks of a specified size.
-- This is a common utility function, implemented using unfoldr.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = takeWhile (not . null) $ unfoldr (\s -> if null s then Nothing else Just (take n s, drop n s)) xs

-- | Translates an RNA sequence into a list of protein names.
-- Returns Nothing if the RNA sequence is invalid (e.g., length not a multiple of 3,
-- or contains an unrecognized codon before a stop codon or end of sequence).
-- Translation stops upon encountering a stop codon.
proteins :: String -> Maybe [String]
proteins rna
    | length rna `mod` 3 /= 0 = Nothing -- RNA length must be a multiple of 3
    | otherwise = translateCodons (chunksOf 3 rna) []
  where
    -- Helper function for recursive translation.
    -- 'acc' accumulates proteins in reverse order for efficient consing.
    translateCodons :: [String] -> [String] -> Maybe [String]
    translateCodons [] acc = Just (reverse acc) -- All codons processed, return accumulated proteins
    translateCodons (codon:rest) acc
        | codon `elem` stopCodons = Just (reverse acc) -- Stop codon encountered, return current proteins
        | otherwise = case Data.Map.lookup codon codonMap of
            Just protein -> translateCodons rest (protein:acc) -- Valid codon, continue translation
            Nothing      -> Nothing -- Unknown codon, invalid RNA sequence

