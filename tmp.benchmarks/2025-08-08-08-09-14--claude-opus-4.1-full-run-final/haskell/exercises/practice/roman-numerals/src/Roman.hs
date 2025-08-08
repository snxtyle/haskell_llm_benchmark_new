module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3999 = Nothing
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman 0 = ""
toRoman n = symbol ++ toRoman (n - value)
  where
    (value, symbol) = largestFit n

largestFit :: Integer -> (Integer, String)
largestFit n
  | n >= 1000 = (1000, "M")
  | n >= 900  = (900, "CM")
  | n >= 500  = (500, "D")
  | n >= 400  = (400, "CD")
  | n >= 100  = (100, "C")
  | n >= 90   = (90, "XC")
  | n >= 50   = (50, "L")
  | n >= 40   = (40, "XL")
  | n >= 10   = (10, "X")
  | n >= 9    = (9, "IX")
  | n >= 5    = (5, "V")
  | n >= 4    = (4, "IV")
  | otherwise = (1, "I")
