module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Text.Printf  -- For formatting strings with printf

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

ohms :: Resistor -> Int
ohms resistor =
  let (c1, c2, c3) = bands resistor
      mainValue = 10 * fromEnum c1 + fromEnum c2  -- First two colors as two-digit number
      zeros = fromEnum c3                        -- Third color as exponent
  in mainValue * (10 ^ zeros)                  -- Multiply by 10 to the power of zeros

label :: Resistor -> String
label resistor =
  let val = ohms resistor
  in if val < 1000
       then show val ++ " ohms"
       else if val < 1000000  -- Check for megaohms
              then
                let kilo = fromIntegral val / 1000 :: Double  -- Convert to kiloohms
                    s = printf "%.1f" kilo                  -- Format with one decimal
                    s' = if last s == '0' && s !! (length s - 2) == '.'  -- Check if it ends with .0
                           then init (init s)                   -- Remove .0
                           else s
                in s' ++ " kiloohms"
              else
                let mega = fromIntegral val / 1000000 :: Double  -- Convert to megaohms
                    s = printf "%.1f" mega                  -- Format with one decimal
                    s' = if last s == '0' && s !! (length s - 2) == '.'  -- Check if it ends with .0
                           then init (init s)                   -- Remove .0
                           else s
                in s' ++ " megaohms"
