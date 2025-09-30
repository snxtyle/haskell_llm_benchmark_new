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
  let value = ohms resistor
  in if value >= 1000000000
     then show (value `div` 1000000000) ++ " gigaohms"
     else if value >= 1000000
          then show (value `div` 1000000) ++ " megaohms"
          else if value >= 1000
               then show (value `div` 1000) ++ " kiloohms"
               else show value ++ " ohms"

ohms :: Resistor -> Int
ohms resistor =
  let (color1, color2, color3) = bands resistor
      digit1 = fromEnum color1
      digit2 = fromEnum color2
      zeros = fromEnum color3
      mainValue = digit1 * 10 + digit2
  in mainValue * (10 ^ zeros)
