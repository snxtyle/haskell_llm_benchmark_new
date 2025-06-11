module ResistorColors (Color(..), Resistor(..), label, ohms) where

-- | The colors that can appear on a resistor.
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
  deriving (Show, Enum, Bounded, Eq)

-- | A resistor is represented by three color bands.
newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

-- | Convert the color bands of a resistor into its resistance value in ohms.
--
--   The first two bands give the significant figures, while the third band
--   gives the number of zeros (i.e. the power of ten) to append.
--
--   Examples:
--
--   >>> ohms (Resistor (Orange, Orange, Black))   -- 33 Ω
--   33
--   >>> ohms (Resistor (Orange, Orange, Red))     -- 3300 Ω
--   3300
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let base   = digit c1 * 10 + digit c2          -- significant figures
      factor = 10 ^ digit c3                     -- multiplier
  in base * factor
  where
    digit :: Color -> Int
    digit = fromEnum

-- | Render a resistor's value as a human-readable label.
--
--   Values divisible by 1 000 are expressed using metric prefixes
--   (kilo, mega, giga, …) as long as they remain whole numbers after
--   successive division by 1 000.
--
--   Examples:
--
--   >>> label (Resistor (Orange, Orange, Black))
--   "33 ohms"
--   >>> label (Resistor (Orange, Orange, Orange))
--   "33 kiloohms"
label :: Resistor -> String
label r =
  let (val, idx) = scale (ohms r) 0
      prefixes   = ["", "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta"]
      prefix     = prefixes !! idx
      unit       = if null prefix then "ohms" else prefix ++ "ohms"
  in show val ++ " " ++ unit
  where
    -- Recursively divide by 1 000 while the value is perfectly divisible.
    scale :: Int -> Int -> (Int, Int)
    scale n i
      | n >= 1000 && n `mod` 1000 == 0 = scale (n `div` 1000) (i + 1)
      | otherwise                      = (n, i)
