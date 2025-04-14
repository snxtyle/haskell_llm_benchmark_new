module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing  -- Invalid input range
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman n = go n values
  where
    values = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), 
              (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), 
              (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
    
    go 0 _ = ""  -- Base case: no more value to convert
    go num [] = ""  -- Should not happen if values list is complete
    go num ((val, sym):xs)
      | num >= val = sym ++ go (num - val) values  -- Subtract and continue
      | otherwise = go num xs  -- Move to next value
