module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing -- Roman numerals are typically for positive integers up to 3999
  | otherwise = Just (toRoman n)

-- Helper function to convert a valid integer to a Roman numeral string
toRoman :: Integer -> String
toRoman n = go n "" romanMap
  where
    -- List of Roman numeral values and their symbols, ordered from largest to smallest.
    -- This order is crucial for the greedy conversion algorithm.
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

    -- Recursive helper to build the Roman numeral string
    go :: Integer -> String -> [(Integer, String)] -> String
    go currentNum acc [] = acc -- Base case: no more symbols to process
    go currentNum acc ((val, sym) : rest)
      | currentNum >= val =
          -- If the current number is greater than or equal to the current symbol's value,
          -- append the symbol(s) and continue with the remainder.
          let numSymbols = currentNum `div` val
              remainder = currentNum `mod` val
              newAcc = acc ++ concat (replicate (fromIntegral numSymbols) sym)
          in go remainder newAcc rest
      | otherwise =
          -- If the current number is less than the current symbol's value,
          -- move to the next symbol in the map.
          go currentNum acc rest
