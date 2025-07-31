module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = 
  case validateDNA xs of
    Left err -> Left err
    Right validDNA -> Right (countNucleotides validDNA)

validateDNA :: String -> Either String String
validateDNA [] = Right []
validateDNA (x:xs) =
  case charToNucleotide x of
    Just _ -> case validateDNA xs of
      Left err -> Left err
      Right rest -> Right (x:rest)
    Nothing -> Left "Invalid nucleotide in DNA string"

charToNucleotide :: Char -> Maybe Nucleotide
charToNucleotide 'A' = Just A
charToNucleotide 'C' = Just C
charToNucleotide 'G' = Just G
charToNucleotide 'T' = Just T
charToNucleotide _ = Nothing

countNucleotides :: String -> Map Nucleotide Int
countNucleotides xs = 
  let initialMap = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
  in foldr (\nucleotideChar acc -> 
      case charToNucleotide nucleotideChar of
        Just n -> Map.adjust (+1) n acc
        Nothing -> acc) initialMap xs
