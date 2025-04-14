module ProteinTranslation(proteins) where

-- Define the mapping of codons to proteins
proteinCodons :: [(String, String)]
proteinCodons = [
  ("AUG", "Methionine"),
  ("UUU", "Phenylalanine"),
  ("UUC", "Phenylalanine"),
  ("UUA", "Leucine"),
  ("UUG", "Leucine"),
  ("UCU", "Serine"),
  ("UCC", "Serine"),
  ("UCA", "Serine"),
  ("UCG", "Serine"),
  ("UAU", "Tyrosine"),
  ("UAC", "Tyrosine"),
  ("UGU", "Cysteine"),
  ("UGC", "Cysteine"),
  ("UGG", "Tryptophan")
  ]

-- Define the STOP codons
stopCodons :: [String]
stopCodons = ["UAA", "UAG", "UGA"]

-- Helper function to split a string into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Implement the proteins function
proteins :: String -> Maybe [String]
proteins rna
  | length rna `mod` 3 /= 0 = Nothing  -- Length must be a multiple of 3
  | otherwise = go (chunksOf 3 rna) []  -- Start processing chunks
  where
    go :: [String] -> [String] -> Maybe [String]
    go [] acc = Just (reverse acc)  -- End of codons, return the accumulated list
    go (codon:rest) acc
      | codon `elem` stopCodons = Just (reverse acc)  -- STOP codon encountered, stop here
      | otherwise = case lookup codon proteinCodons of
          Just protein -> go rest (protein : acc)  -- Valid protein codon, add to accumulator and continue
          Nothing -> Nothing  -- Invalid codon, return Nothing
