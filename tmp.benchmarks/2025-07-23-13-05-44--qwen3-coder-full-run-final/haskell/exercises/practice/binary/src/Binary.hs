module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs = toDecimalHelper xs 0
  where
    toDecimalHelper [] acc = acc
    toDecimalHelper (y:ys) acc
      | y == '0' = toDecimalHelper ys (acc * 2)
      | y == '1' = toDecimalHelper ys (acc * 2 + 1)
      | otherwise = 0  -- Invalid character, return 0
