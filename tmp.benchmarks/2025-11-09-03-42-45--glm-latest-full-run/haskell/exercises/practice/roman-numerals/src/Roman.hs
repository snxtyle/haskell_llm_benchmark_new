module Roman (numerals) where

-- | Convert an integer to its Roman numeral representation.
-- Returns Nothing for numbers outside the standard Roman numeral range (1-3999).
numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing
  | otherwise          = Just (toRoman n)

-- | A list of Roman numeral mappings, ordered from largest to smallest.
-- This includes the subtractive forms.
romanMap :: [(Integer, String)]
romanMap = [ (1000, "M")
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

-- | Helper function to perform the conversion using a greedy algorithm.
toRoman :: Integer -> String
toRoman 0 = ""
toRoman n = let (value, numeral) = findNumeral n
            in numeral ++ toRoman (n - value)
  where
    -- Find the largest numeral value that fits into the given number.
    findNumeral :: Integer -> (Integer, String)
    findNumeral num = head $ filter ((<= num) . fst) romanMap
