module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Data.List   (isSuffixOf)
import Numeric     (showFFloat)

-- | All available colors and the digit they represent.
data Color =
    Black  -- 0
  | Brown  -- 1
  | Red    -- 2
  | Orange -- 3
  | Yellow -- 4
  | Green  -- 5
  | Blue   -- 6
  | Violet -- 7
  | Grey   -- 8
  | White  -- 9
  deriving (Show, Enum, Bounded)

-- | A resistor consists of exactly three color bands.
newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

-- | Convert a resistor into a human‑readable label such as
--   "33 ohms", "4.7 kiloohms", "5 megaohms", ...
label :: Resistor -> String
label r =
  let totalOhms = fromIntegral (ohms r) :: Double
      (val, unit) = chooseUnit totalOhms ["ohms", "kiloohms", "megaohms", "gigaohms"]
  in formatValue val ++ " " ++ unit

-- | Calculate the resistance in ohms represented by the three color bands.
ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let significant = colorValue c1 * 10 + colorValue c2
      multiplier  = 10 ^ colorValue c3
  in significant * multiplier

-- Helpers ------------------------------------------------------------

-- | Numeric value associated with a color.
colorValue :: Color -> Int
colorValue = fromEnum

-- | Recursively divide by 1000 until the value fits the current unit.
chooseUnit :: Double -> [String] -> (Double, String)
chooseUnit value (u:us)
  | value >= 1000 && not (null us) = chooseUnit (value / 1000) us
  | otherwise                      = (value, u)
chooseUnit value [] = (value, "ohms") -- Fallback (shouldn't happen).

-- | Format a number:
--   * with one decimal place if needed (e.g. 1.1)
--   * without trailing ".0" for whole numbers (e.g. 33)
formatValue :: Double -> String
formatValue v =
  let str = showFFloat (Just 1) v ""
  in if ".0" `isSuffixOf` str
        then take (length str - 2) str
        else str
