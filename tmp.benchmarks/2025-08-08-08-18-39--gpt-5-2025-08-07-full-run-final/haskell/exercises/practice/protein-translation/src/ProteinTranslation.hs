module ProteinTranslation(proteins) where

data Outcome = Stop | Yield String

proteins :: String -> Maybe [String]
proteins rna = do
  codons <- chunksOf3 rna
  translate codons
  where
    -- Break the input into groups of three characters.
    -- Return Nothing if there are leftover nucleotides (< 3).
    chunksOf3 :: String -> Maybe [String]
    chunksOf3 [] = Just []
    chunksOf3 (a:b:c:rest) = ([a, b, c] :) <$> chunksOf3 rest
    chunksOf3 _ = Nothing

    translateCodon :: String -> Maybe Outcome
    translateCodon c
      | c == "AUG" = Just (Yield "Methionine")
      | c `elem` ["UUU", "UUC"] = Just (Yield "Phenylalanine")
      | c `elem` ["UUA", "UUG"] = Just (Yield "Leucine")
      | c `elem` ["UCU", "UCC", "UCA", "UCG"] = Just (Yield "Serine")
      | c `elem` ["UAU", "UAC"] = Just (Yield "Tyrosine")
      | c `elem` ["UGU", "UGC"] = Just (Yield "Cysteine")
      | c == "UGG" = Just (Yield "Tryptophan")
      | c `elem` ["UAA", "UAG", "UGA"] = Just Stop
      | otherwise = Nothing

    translate :: [String] -> Maybe [String]
    translate [] = Just []
    translate (c:cs) =
      case translateCodon c of
        Nothing        -> Nothing
        Just Stop      -> Just []
        Just (Yield p) -> (p :) <$> translate cs
