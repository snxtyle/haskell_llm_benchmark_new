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

-- Helper function to get the integer value of a color
colorValue :: Color -> Int
colorValue = fromEnum

ohms :: Resistor -> Int
ohms resistor =
  let (c1, c2, c3) = bands resistor
      val1 = colorValue c1
      val2 = colorValue c2
      numZeros = colorValue c3
      
      baseValue = val1 * 10 + val2
  in baseValue * (10 ^ numZeros)

label :: Resistor -> String
label resistor =
  let o = ohms resistor
      gigaOhmsThreshold = 1000000000
      megaOhmsThreshold = 1000000
      kiloOhmsThreshold = 1000
  in if o >= gigaOhmsThreshold && o `mod` gigaOhmsThreshold == 0
     then show (o `div` gigaOhmsThreshold) ++ " gigaohms"
     else if o >= megaOhmsThreshold && o `mod` megaOhmsThreshold == 0
     then show (o `div` megaOhmsThreshold) ++ " megaohms"
     else if o >= kiloOhmsThreshold && o `mod` kiloOhmsThreshold == 0
     then show (o `div` kiloOhmsThreshold) ++ " kiloohms"
     else show o ++ " ohms"
