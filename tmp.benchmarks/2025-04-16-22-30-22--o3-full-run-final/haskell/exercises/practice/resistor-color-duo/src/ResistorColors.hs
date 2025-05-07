module ResistorColors (Color(..), value) where

-- | Represents the standard color bands found on resistors.
--   The order of the constructors is important: deriving `Enum`
--   will assign 0 to Black, 1 to Brown, …, 9 to White, exactly
--   matching the resistor‑color code specification.
data Color
  = Black
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

-- | Given a pair of color bands (the first and second band on
--   a resistor), return the two‑digit numeric value represented
--   by those colors.
--
--   Example:
--     value (Brown, Green)  == 15
--     value (Red,   Black)  == 20
value :: (Color, Color) -> Int
value (c1, c2) = fromEnum c1 * 10 + fromEnum c2
