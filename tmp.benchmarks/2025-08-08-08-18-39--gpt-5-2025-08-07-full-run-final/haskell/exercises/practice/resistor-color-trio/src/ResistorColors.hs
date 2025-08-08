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

-- Convert a color band to its digit value
toValue :: Color -> Int
toValue = fromEnum

-- Compute the resistance in ohms
ohms :: Resistor -> Int
ohms (Resistor (a, b, c)) =
  let mainValue = 10 * toValue a + toValue b
      multiplier = 10 ^ toValue c
  in mainValue * multiplier

-- Produce a human-readable label using metric prefixes where appropriate
label :: Resistor -> String
label r =
  let total = ohms r
      (v, u) = scale total 0
  in show v ++ " " ++ u
  where
    units :: [String]
    units = ["ohms", "kiloohms", "megaohms", "gigaohms"]

    -- scale down by 1000 while divisible, moving through units
    scale :: Int -> Int -> (Int, String)
    scale n idx
      | n >= 1000 && n `mod` 1000 == 0 && idx < length units - 1
          = scale (n `div` 1000) (idx + 1)
      | otherwise = (n, units !! idx)
