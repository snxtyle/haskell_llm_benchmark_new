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
  deriving (Show, Eq, Enum, Bounded) -- Added Eq for completeness

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

-- | Calculate the resistance value in ohms.
-- The first two bands represent the significant digits,
-- and the third band represents the number of zeros to follow.
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let val1 = fromEnum c1 -- Get integer value for the first band (0-9)
      val2 = fromEnum c2 -- Get integer value for the second band (0-9)
      zeros = fromEnum c3 -- Get integer value for the third band (number of zeros)
      -- Combine the first two digits (e.g., Brown, Green -> 15)
      baseValue = val1 * 10 + val2
      -- Multiply by 10 raised to the power of the third band's value
      -- (e.g., baseValue 15, zeros 3 (Orange) -> 15 * 10^3 = 15000)
  in baseValue * (10 ^ zeros)

-- | Determine the appropriate metric prefix and scaled value based on the ohms value.
-- Prefixes are only used for exact multiples (e.g., 1000 ohms -> 1 kiloohm, but 1100 ohms -> 1100 ohms).
determinePrefix :: Int -> (Int, String)
determinePrefix o
    -- Check for gigaohms: must be >= 1 Gigaohm and perfectly divisible
  | o >= 1000000000 && o `rem` 1000000000 == 0 = (o `quot` 1000000000, "gigaohms")
    -- Check for megaohms: must be >= 1 Megaohm and perfectly divisible
  | o >= 1000000 && o `rem` 1000000 == 0 = (o `quot` 1000000, "megaohms")
    -- Check for kiloohms: must be >= 1 Kiloohm and perfectly divisible
  | o >= 1000 && o `rem` 1000 == 0 = (o `quot` 1000, "kiloohms")
    -- Otherwise, use ohms
  | otherwise = (o, "ohms")

-- | Generate the human-readable label string for the resistor,
-- including the value and unit with the appropriate metric prefix.
label :: Resistor -> String
label resistor =
  let value = ohms resistor -- Calculate the raw ohms value
      (scaledValue, prefix) = determinePrefix value -- Determine the scaled value and prefix unit
  in show scaledValue ++ " " ++ prefix -- Format the final string (e.g., "33 kiloohms")

