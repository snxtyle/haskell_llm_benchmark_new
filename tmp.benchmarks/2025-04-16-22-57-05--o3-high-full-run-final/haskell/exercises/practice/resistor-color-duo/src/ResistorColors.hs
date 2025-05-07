module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show, Enum, Bounded)

-- | Convert the first two color bands of a resistor to their numeric value.
--   Example: value (Brown, Green) == 15
value :: (Color, Color) -> Int
value (a, b) = fromEnum a * 10 + fromEnum b
