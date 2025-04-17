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

-- Compute the resistance in ohms as an integer
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let digits     = fromEnum c1 * 10 + fromEnum c2
      multiplier = 10 ^ fromEnum c3
  in digits * multiplier

-- Produce a human-readable label with the appropriate metric prefix
label :: Resistor -> String
label r =
  let v = ohms r
  in if v < 1000
       then show v ++ " ohms"
       else if v < 1000000
         then show (v `div` 1000) ++ " kiloohms"
         else show (v `div` 1000000) ++ " megaohms"
