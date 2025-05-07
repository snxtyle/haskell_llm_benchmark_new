module Roman (numerals) where

-- A list of Roman numeral values and their string representations,
-- ordered from largest to smallest. This order is crucial for the
-- greedy conversion algorithm. It includes subtractive pairs
-- (e.g., CM for 900, CD for 400) to ensure correct and standard form.
romanMap :: [(Integer, String)]
romanMap =
  [ (1000, "M")
  , (900, "CM")
  , (500, "D")
  , (400, "CD")
  , (100, "C")
  , (90, "XC")
  , (50, "L")
  , (40, "XL")
  , (10, "X")
  , (9, "IX")
  , (5, "V")
  , (4, "IV")
  , (1, "I")
  ]

-- | Converts an Integer to its Roman numeral representation.
-- Returns Nothing if the number is outside the range 1-3999.
numerals :: Integer -> Maybe String
numerals n
  -- Roman numerals are typically positive and standard notation handles up to 3999.
  | n <= 0 || n > 3999 = Nothing
  | otherwise          = Just (toRoman n romanMap)

-- Helper function to perform the conversion using a greedy approach.
toRoman :: Integer -> [(Integer, String)] -> String
toRoman 0 _      = ""  -- Base case: if the number is 0, conversion is complete.
toRoman _ []     = ""  -- Base case: if the map is exhausted (should not happen for n > 0).
toRoman num ((val, sym):remainingMap)
  -- If the current number is greater than or equal to the current Roman value,
  -- append the symbol and subtract the value from the number.
  -- Crucially, pass the *same* map element `(val, sym):remainingMap` to allow
  -- for repeated symbols (e.g., III for 3, XX for 20).
  | num >= val = sym ++ toRoman (num - val) ((val, sym):remainingMap)
  -- Otherwise, the current Roman value is too large; try the next one in the map.
  | otherwise  = toRoman num remainingMap
