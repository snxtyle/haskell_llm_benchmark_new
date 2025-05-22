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
label resistor = formatOhms (ohms resistor)
  where
    formatOhms :: Int -> String
    formatOhms value
      | value >= 1000000000 = show (value `div` 1000000000) ++ " gigaohms"
      | value >= 1000000 = show (value `div` 1000000) ++ " megaohms"
      | value >= 1000 = show (value `div` 1000) ++ " kiloohms"
      | otherwise = show value ++ " ohms"

ohms :: Resistor -> Int
ohms (Resistor (color1, color2, color3)) = 
    let digit1 = fromEnum color1
        digit2 = fromEnum color2
        zeros = fromEnum color3
        mainValue = digit1 * 10 + digit2
    in mainValue * (10 ^ zeros)
