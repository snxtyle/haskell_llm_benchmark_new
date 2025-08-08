module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 = Nothing
  | n > 3999 = Nothing
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman n = go n romanNumerals
  where
    go :: Integer -> [(Integer, String)] -> String
    go 0 _ = ""
    go m ((v, s):rest)
      | m >= v = s ++ go (m - v) ((v, s):rest)
      | otherwise = go m rest
    go _ [] = ""

romanNumerals :: [(Integer, String)]
romanNumerals =
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
