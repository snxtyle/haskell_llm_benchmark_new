module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldr processChar (Right initialMap)
  where
    initialMap :: Map Nucleotide Int
    initialMap = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

    processChar :: Char -> Either String (Map Nucleotide Int) -> Either String (Map Nucleotide Int)
    processChar char acc =
      case acc of
        Left err -> Left err
        Right counts ->
          case char of
            'A' -> Right $ adjust (+1) A counts
            'C' -> Right $ adjust (+1) C counts
            'G' -> Right $ adjust (+1) G counts
            'T' -> Right $ adjust (+1) T counts
            _   -> Left "Invalid nucleotide"
