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

-- Map a color to its digit value (0-9)
colorValue :: Color -> Int
colorValue = fromEnum

-- Compute the resistance in ohms from the three bands
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let sig = colorValue c1 * 10 + colorValue c2
      multiplier = colorValue c3
  in sig * (10 ^ multiplier)

-- Format the resistance with appropriate metric prefix
label :: Resistor -> String
label r =
  let value = ohms r
  in formatOhms value

-- Choose largest suitable unit where value is divisible by the unit factor
formatOhms :: Int -> String
formatOhms 0 = "0 ohms"
formatOhms n
  | n `mod` giga == 0 = show (n `div` giga) ++ " gigaohms"
  | n `mod` mega == 0 = show (n `div` mega) ++ " megaohms"
  | n `mod` kilo == 0 = show (n `div` kilo) ++ " kiloohms"
  | otherwise         = show n ++ " ohms"
  where
    kilo = 1000
    mega = 1000 * kilo
    giga = 1000 * mega
