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

-- Helper function to get the numerical value of a Color
colorValue :: Color -> Int
colorValue = fromEnum

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
    let
        val1 = colorValue c1
        val2 = colorValue c2
        multiplierPower = colorValue c3
        baseValue = val1 * 10 + val2
        multiplier = 10 ^ multiplierPower
    in
        baseValue * multiplier

label :: Resistor -> String
label resistor =
    let
        totalOhms = ohms resistor
    in
        formatOhms totalOhms

-- Helper function to format the ohms value with appropriate metric prefixes
formatOhms :: Int -> String
formatOhms n
    | n < 1000 = show n ++ " ohms"
    | n < 1000000 = show (n `div` 1000) ++ " kiloohms"
    | n < 1000000000 = show (n `div` 1000000) ++ " megaohms"
    | otherwise = show (n `div` 1000000000) ++ " gigaohms"
