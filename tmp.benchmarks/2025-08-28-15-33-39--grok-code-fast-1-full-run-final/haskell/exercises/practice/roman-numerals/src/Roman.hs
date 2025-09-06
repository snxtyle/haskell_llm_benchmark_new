module Roman (numerals) where

romanNumerals :: [(Integer, String)]
romanNumerals = [
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

convert :: Integer -> String
convert 0 = ""
convert n = let (val, sym) = head [ (v, s) | (v, s) <- romanNumerals, v <= n ]
            in sym ++ convert (n - val)

numerals :: Integer -> Maybe String
numerals n
    | n <= 0 = Nothing
    | n > 3999 = Nothing
    | otherwise = Just (convert n)
