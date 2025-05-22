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
colorValue color = fromEnum color

-- Calculate the resistance value in ohms
ohms :: Resistor -> Int
ohms (Resistor (first, second, third)) = 
  let mainValue = colorValue first * 10 + colorValue second
      multiplier = 10 ^ colorValue third
  in mainValue * multiplier

-- Format the resistance value with appropriate metric prefix
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
