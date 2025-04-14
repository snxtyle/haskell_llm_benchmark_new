module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing
  | otherwise = Just $ toRoman n
  where
    toRoman 0 = ""
    toRoman num =
      let (val, sym) = largestSymbol num
      in sym ++ toRoman (num - val)
    
    largestSymbol num =
      head $ dropWhile (\(val, _) -> val > num) romanSymbols
    
    romanSymbols = [
        (1000, "M"),
        (900, "CM"),
        (500, "D"),
        (400, "CD"),
        (100, "C"),
        (90, "XC"),
        (50, "L"),
        (40, "XL"),
        (10, "X"),
        (9, "IX"),
        (5, "V"),
        (4, "IV"),
        (1, "I")
      ]
