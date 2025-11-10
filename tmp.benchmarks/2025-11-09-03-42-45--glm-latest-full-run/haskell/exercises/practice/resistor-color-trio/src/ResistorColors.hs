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

-- | Calculate the resistance value in ohms.
-- The first two bands are the significant figures, and the third is the multiplier.
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let significant = (fromEnum c1) * 10 + (fromEnum c2)
      multiplier = 10 ^ (fromEnum c3)
  in significant * multiplier

-- | Create a human-readable label for the resistor's value.
-- The label includes the value and the appropriate unit (ohms, kiloohms, etc.).
label :: Resistor -> String
label resistor =
  let value = ohms resistor
  in if value >= 1000000000
     then show (value `div` 1000000000) ++ " gigaohms"
     else if value >= 1000000
          then show (value `div` 1000000) ++ " megaohms"
          else if value >= 1000
               then show (value `div` 1000) ++ " kiloohms"
               else show value ++ " ohms"
