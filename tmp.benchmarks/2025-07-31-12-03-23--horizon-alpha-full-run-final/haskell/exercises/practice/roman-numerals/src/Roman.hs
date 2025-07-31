module Roman (numerals) where

-- Convert an Integer to a Roman numeral.
-- Returns Nothing for non-positive numbers or numbers > 3999 (traditional range).
numerals :: Integer -> Maybe String
numerals n
  | n <= 0 = Nothing
  | n > 3999 = Nothing
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman = go romanTable
  where
    go _ 0 = ""
    go [] _ = "" -- Should not occur within valid range
    go table@((val, sym):rest) n
      | n >= val  = sym ++ go table (n - val)
      | otherwise = go rest n

-- Ordered from largest to smallest, including subtractive pairs
romanTable :: [(Integer, String)]
romanTable =
  [ (1000, "M")
  , (900,  "CM")
  , (500,  "D")
  , (400,  "CD")
  , (100,  "C")
  , (90,   "XC")
  , (50,   "L")
  , (40,   "XL")
  , (10,   "X")
  , (9,    "IX")
  , (5,    "V")
  , (4,    "IV")
  , (1,    "I")
  ]
