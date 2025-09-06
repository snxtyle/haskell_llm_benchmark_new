module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 = Nothing
  | n > 3999 = Nothing
  | otherwise = Just (convert n)
  where
    convert 0 = ""
    convert x
      | x >= 1000 = "M" ++ convert (x - 1000)
      | x >= 900 = "CM" ++ convert (x - 900)
      | x >= 500 = "D" ++ convert (x - 500)
      | x >= 400 = "CD" ++ convert (x - 400)
      | x >= 100 = "C" ++ convert (x - 100)
      | x >= 90 = "XC" ++ convert (x - 90)
      | x >= 50 = "L" ++ convert (x - 50)
      | x >= 40 = "XL" ++ convert (x - 40)
      | x >= 10 = "X" ++ convert (x - 10)
      | x >= 9 = "IX" ++ convert (x - 9)
      | x >= 5 = "V" ++ convert (x - 5)
      | x >= 4 = "IV" ++ convert (x - 4)
      | otherwise = "I" ++ convert (x - 1)
