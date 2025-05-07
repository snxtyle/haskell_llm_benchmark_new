module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n >= 4000 = Nothing
  | otherwise           = Just (convert n)
  where
    -- Greedily consume the highest possible Roman value
    convert 0 = ""
    convert x =
      let (v, s) = head $ filter (\(v, _) -> x >= v) values
      in  s ++ convert (x - v)

    values :: [(Integer, String)]
    values =
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
