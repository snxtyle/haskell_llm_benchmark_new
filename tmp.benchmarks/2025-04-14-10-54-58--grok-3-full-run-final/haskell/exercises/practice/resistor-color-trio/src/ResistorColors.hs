module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor = formatValue (ohms resistor)

ohms :: Resistor -> Int
ohms (Resistor (first, second, multiplier)) = 
  let
    value = (fromEnum first * 10 + fromEnum second)
    zeros = fromEnum multiplier
  in
    value * (10 ^ zeros)

formatValue :: Int -> String
formatValue value
  | value >= 10^9 = show (value `div` 10^9) ++ " gigaohms"
  | value >= 10^6 = show (value `div` 10^6) ++ " megaohms"
  | value >= 10^3 = show (value `div` 10^3) ++ " kiloohms"
  | otherwise     = show value ++ " ohms"
