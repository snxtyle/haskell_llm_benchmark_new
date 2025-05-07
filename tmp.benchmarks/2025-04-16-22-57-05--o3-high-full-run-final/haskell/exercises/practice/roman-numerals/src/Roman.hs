module Roman (numerals) where

-- | Convert an 'Integer' to its Roman numeral representation,
--   if the value is within the traditional range (1‒3999).
--
--   Returns 'Nothing' for values that cannot be represented.
numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n >= 4000 = Nothing
  | otherwise           = Just (toRoman n)
  where
    -- Greedy conversion: always pick the largest possible numeral,
    -- subtract its value, and continue.
    toRoman 0 = ""
    toRoman x =
      let (val, sym) = head $ dropWhile (\(v, _) -> v > x) romanPairs
       in sym ++ toRoman (x - val)

    -- Pairs of (value, symbol) sorted from largest to smallest.
    romanPairs :: [(Integer, String)]
    romanPairs =
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
