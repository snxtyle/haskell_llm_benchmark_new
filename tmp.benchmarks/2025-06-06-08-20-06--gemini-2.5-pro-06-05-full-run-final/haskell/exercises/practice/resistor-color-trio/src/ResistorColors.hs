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
label resistor = formatOhms (ohms resistor)
  where
    formatOhms o
      | o > 0 && o `rem` 1000000000 == 0 = show (o `div` 1000000000) ++ " gigaohms"
      | o > 0 && o `rem` 1000000 == 0 = show (o `div` 1000000) ++ " megaohms"
      | o > 0 && o `rem` 1000 == 0 = show (o `div` 1000) ++ " kiloohms"
      | otherwise = show o ++ " ohms"

ohms :: Resistor -> Int
ohms (Resistor { bands = (c1, c2, c3) }) =
  (fromEnum c1 * 10 + fromEnum c2) * (10 ^ fromEnum c3)
