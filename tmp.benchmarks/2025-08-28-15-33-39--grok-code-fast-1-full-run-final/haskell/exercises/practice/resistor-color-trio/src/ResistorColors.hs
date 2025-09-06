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

label :: Resistor -> String
label resistor = let (c1, c2, c3) = bands resistor
                     digits = fromEnum c1 * 10 + fromEnum c2
                     exponent = fromEnum c3
                     value = digits * (10 ^ exponent)
                     (num, level) = go value 0
                     units = ["ohms", "kiloohms", "megaohms", "gigaohms"]
                     unit = units !! level
                 in show num ++ " " ++ unit
  where
    go x lvl
      | x >= 1000 && x `mod` 1000 == 0 = go (x `div` 1000) (lvl + 1)
      | otherwise = (x, lvl)

ohms :: Resistor -> Int
ohms resistor = let (c1, c2, c3) = bands resistor
                in (fromEnum c1 * 10 + fromEnum c2) * (10 ^ fromEnum c3)
