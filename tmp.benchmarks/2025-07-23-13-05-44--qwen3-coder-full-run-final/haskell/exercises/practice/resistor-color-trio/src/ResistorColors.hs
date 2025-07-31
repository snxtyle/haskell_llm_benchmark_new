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
  let (first, second, third) = bands resistor
      value = ohms resistor
      (displayValue, unit) = formatValue value
  in show displayValue ++ " " ++ unit

ohms :: Resistor -> Int
ohms resistor = 
  let (first, second, third) = bands resistor
      mainValue = fromEnum first * 10 + fromEnum second
      zeros = fromEnum third
  in mainValue * (10 ^ zeros)

formatValue :: Int -> (Int, String)
formatValue value
  | value >= 1000000000 = (value `div` 1000000000, "gigaohms")
  | value >= 1000000 = (value `div` 1000000, "megaohms")
  | value >= 1000 = (value `div` 1000, "kiloohms")
  | otherwise = (value, "ohms")
