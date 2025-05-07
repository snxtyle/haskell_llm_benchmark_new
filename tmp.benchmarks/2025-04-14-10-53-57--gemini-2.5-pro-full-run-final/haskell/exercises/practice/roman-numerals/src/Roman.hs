module Roman (numerals) where

-- Define the mapping of values to Roman numeral symbols, ordered from largest to smallest.
-- Includes subtractive forms like CM (900), CD (400), XC (90), XL (40), IX (9), IV (4).
romanMap :: [(Integer, String)]
romanMap =
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

-- | Converts an Integer to its Roman numeral representation, if possible.
-- Returns Nothing for numbers outside the traditional range (1 to 3999).
numerals :: Integer -> Maybe String
numerals n
  -- Guard against invalid input numbers (0, negative, or >= 4000)
  | n <= 0 || n >= 4000 = Nothing
  -- Otherwise, proceed with the conversion
  | otherwise           = Just $ convert n romanMap
  where
    -- Recursive helper function to build the Roman numeral string.
    convert :: Integer -> [(Integer, String)] -> String
    -- Base case: If the number is 0, we're done.
    convert 0 _      = ""
    -- Base case: If we run out of symbols (shouldn't happen with n > 0 and the current map).
    convert _ []     = ""
    -- Recursive step: Process the current number with the head of the mapping list.
    convert num ((val, sym):rest)
      -- If the current number is greater than or equal to the symbol's value,
      -- append the symbol and recurse with the decremented number and the *same* mapping list
      -- (to handle repetitions like III).
      | num >= val = sym ++ convert (num - val) ((val, sym):rest)
      -- Otherwise, move to the next smaller symbol in the mapping list.
      | otherwise  = convert num rest
