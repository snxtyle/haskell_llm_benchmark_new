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

-- Convert a color to its numeric value
colorValue :: Color -> Int
colorValue = fromEnum

-- Calculate the resistance in ohms
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = 
    let firstDigit = colorValue c1
        secondDigit = colorValue c2
        multiplier = 10 ^ colorValue c3
    in (firstDigit * 10 + secondDigit) * multiplier

-- Format the resistance with appropriate unit
label :: Resistor -> String
label resistor = 
    let resistance = ohms resistor
    in formatResistance resistance

-- Format resistance with appropriate metric prefix
formatResistance :: Int -> String
formatResistance resistance
    | resistance < 1000 = show resistance ++ " ohms"
    | resistance < 1000000 = show (resistance `div` 1000) ++ " kiloohms"
    | resistance < 1000000000 = show (resistance `div` 1000000) ++ " megaohms"
    | otherwise = show (resistance `div` 1000000000) ++ " gigaohms"
