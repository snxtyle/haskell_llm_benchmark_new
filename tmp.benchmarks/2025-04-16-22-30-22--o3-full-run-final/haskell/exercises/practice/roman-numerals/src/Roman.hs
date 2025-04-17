module Roman (numerals) where

-- | Convert an Integer to its Roman numeral representation.
--   Returns 'Nothing' for values that cannot be represented
--   using standard (classical) Roman numerals (i.e. anything
--   outside the range 1–3999).
numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n >= 4000 = Nothing
  | otherwise           = Just (toRoman n)

-- Greedy decomposition using the standard Roman numeral symbols.
toRoman :: Integer -> String
toRoman 0 = ""
toRoman value =
    let (num, sym) = head $ dropWhile ((> value) . fst) numeralTable
    in sym ++ toRoman (value - num)

-- Table of numeral values in descending order, including the
-- subtractive combinations required to avoid four consecutive
-- repetitions of the same symbol.
numeralTable :: [(Integer, String)]
numeralTable =
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
