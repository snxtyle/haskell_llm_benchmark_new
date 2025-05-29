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
label resistor = 
    let total = ohms resistor
    in if total == 0
        then "0 ohms"
        else if total `mod` 1000000000 == 0
            then show (total `div` 1000000000) ++ " gigaohms"
        else if total `mod` 1000000 == 0
            then show (total `div` 1000000) ++ " megaohms"
        else if total `mod` 1000 == 0
            then show (total `div` 1000) ++ " kiloohms"
        else show total ++ " ohms"

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = 
    let digit1 = fromEnum c1
        digit2 = fromEnum c2
        expValue = fromEnum c3
    in (10 * digit1 + digit2) * (10 ^ expValue)
