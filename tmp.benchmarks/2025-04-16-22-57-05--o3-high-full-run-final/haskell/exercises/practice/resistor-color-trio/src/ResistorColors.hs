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

-- | Translate a colour to its corresponding digit.
colorDigit :: Color -> Int
colorDigit = fromEnum

-- | Calculate the resistance of a resistor in ohms.
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let base   = colorDigit c1 * 10 + colorDigit c2
      factor = 10 ^ colorDigit c3
  in base * factor

-- | Create a human‑readable label for a resistor, using the appropriate
--   metric prefix when necessary.
label :: Resistor -> String
label r =
  let value = ohms r
      (scaled, unit)
        | value >= 1000000000 = (value `div` 1000000000, "gigaohms")
        | value >= 1000000    = (value `div` 1000000   , "megaohms")
        | value >= 1000       = (value `div` 1000      , "kiloohms")
        | otherwise           = (value                 , "ohms")
  in show scaled ++ " " ++ unit
