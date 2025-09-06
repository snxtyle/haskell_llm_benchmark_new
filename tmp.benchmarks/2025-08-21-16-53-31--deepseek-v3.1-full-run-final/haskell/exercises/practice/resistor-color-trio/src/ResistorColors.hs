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
  deriving (Show, Enum, Bounded, Eq)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

colorValue :: Color -> Int
colorValue color = fromEnum color

label :: Resistor -> String
label (Resistor (c1, c2, c3)) = 
    let mainValue = colorValue c1 * 10 + colorValue c2
        zeros = colorValue c3
        totalOhms = mainValue * (10 ^ zeros)
    in formatResistance totalOhms

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = 
    let mainValue = colorValue c1 * 10 + colorValue c2
        zeros = colorValue c3
    in mainValue * (10 ^ zeros)

formatResistance :: Int -> String
formatResistance ohms
    | ohms >= 1000000000 = show (ohms `div` 1000000000) ++ " gigaohms"
    | ohms >= 1000000    = show (ohms `div` 1000000) ++ " megaohms"
    | ohms >= 1000       = show (ohms `div` 1000) ++ " kiloohms"
    | otherwise          = show ohms ++ " ohms"
