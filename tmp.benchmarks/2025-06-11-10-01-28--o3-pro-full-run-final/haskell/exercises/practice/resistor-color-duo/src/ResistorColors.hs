module ResistorColors (Color(..), value) where

-- | Enumeration of the standard resistor band colours in the order that
--   corresponds to their numeric encoding (black = 0 through white = 9).
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

-- | Given a pair of colour bands, return the two–digit value they encode.
--   The first colour supplies the tens digit and the second the ones digit.
--
--   >>> value (Brown, Green)    -- 1 5
--   15
--   >>> value (Brown, Green) == value (Brown, Green)  -- idempotent
--   True
value :: (Color, Color) -> Int
value (a, b) = 10 * digit a + digit b
  where
    digit :: Color -> Int
    digit = fromEnum
