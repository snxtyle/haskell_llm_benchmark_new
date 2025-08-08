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
label r = show scaledValue ++ " " ++ unit
  where
    total = ohms r
    units = ["ohms", "kiloohms", "megaohms", "gigaohms"]
    (scaledValue, unitIndex) = scale total 0
    unit = units !! unitIndex

    scale :: Int -> Int -> (Int, Int)
    scale v i
      | v >= 1000 && v `mod` 1000 == 0 && i < length units - 1 = scale (v `div` 1000) (i + 1)
      | otherwise = (v, i)

ohms :: Resistor -> Int
ohms r = mainValue * (10 ^ zeros)
  where
    (c1, c2, c3) = bands r
    d1 = fromEnum c1
    d2 = fromEnum c2
    zeros = fromEnum c3
    mainValue = 10 * d1 + d2
