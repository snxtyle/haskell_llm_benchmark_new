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

ohms :: Resistor -> Integer
ohms (Resistor (c1, c2, c3)) = 
    let d1 = toInteger (fromEnum c1)
        d2 = toInteger (fromEnum c2)
        exponent = fromEnum c3
        base = d1 * 10 + d2
    in base * (10 ^ exponent)

label :: Resistor -> String
label resistor = 
    let value = ohms resistor
    in if value == 0
        then "0 ohms"
        else case () of
            _ | value >= 1000000000 && value `mod` 1000000000 == 0 -> 
                  show (value `div` 1000000000) ++ " gigaohms"
              | value >= 1000000 && value `mod` 1000000 == 0 -> 
                  show (value `div` 1000000) ++ " megaohms"
              | value >= 1000 && value `mod` 1000 == 0 -> 
                  show (value `div` 1000) ++ " kiloohms"
              | otherwise -> show value ++ " ohms"
