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

colorValue :: Color -> Int
colorValue c = fromEnum c

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let mainValue = colorValue c1 * 10 + colorValue c2
      multiplier = 10 ^ colorValue c3
  in mainValue * multiplier

label :: Resistor -> String
label r =
  let value = ohms r
  in case () of
       _ | value < 1000 -> show value ++ " ohms"
         | value < 1000000 -> show (value `div` 1000) ++ " kiloohms"
         | otherwise -> show (value `div` 1000000) ++ " megaohms"
