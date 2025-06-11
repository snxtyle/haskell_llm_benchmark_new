module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n > 0 && n < 4000 = Just (toRoman n)
  | otherwise         = Nothing

toRoman :: Integer -> String
toRoman n = go n romanMap
  where
    go 0 _ = ""
    go num allRomans@((val, sym):xs)
      | num >= val = sym ++ go (num - val) allRomans
      | otherwise  = go num xs
    go _ [] = "" -- Should not be reached with valid input

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
