module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3999 = Nothing
  | otherwise = Just (go n pairs)
  where
    pairs = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
    go 0 _ = ""
    go _ [] = ""
    go num ((val, sym):rest)
      | num >= val = sym ++ go (num - val) ((val, sym):rest)
      | otherwise = go num rest
