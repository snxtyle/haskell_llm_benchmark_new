module Roman (numerals) where

-- | Convert an Integer to its Roman numeral representation.
--   Returns Nothing for numbers outside the traditional range
--   (1 .. 3999).  Otherwise returns `Just` the Roman numeral.
numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n >= 4000 = Nothing
  | otherwise           = Just (toRoman n)
  where
    -- Table of numeral values in descending order, including the
    -- subtractive forms mandated by the “no more than three in a row” rule.
    table :: [(Integer, String)]
    table =
      [ (1000, "M")
      , (900 , "CM")
      , (500 , "D")
      , (400 , "CD")
      , (100 , "C")
      , (90  , "XC")
      , (50  , "L")
      , (40  , "XL")
      , (10  , "X")
      , (9   , "IX")
      , (5   , "V")
      , (4   , "IV")
      , (1   , "I")
      ]

    -- Greedy conversion: subtract the largest possible numeral value
    -- and append its symbol, repeating until the number is reduced to 0.
    toRoman :: Integer -> String
    toRoman 0 = ""
    toRoman m = case dropWhile ((> m) . fst) table of
                  ((v,s):_) -> s ++ toRoman (m - v)
                  _         -> error "unreachable: table covers all positive integers below 4000"
