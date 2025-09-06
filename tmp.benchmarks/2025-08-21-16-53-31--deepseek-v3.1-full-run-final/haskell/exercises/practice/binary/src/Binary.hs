module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
    | all (`elem` "01") xs = convert (reverse xs) 0
    | otherwise = 0
  where
    convert [] _ = 0
    convert (x:xs) power = digitValue x * (2 ^ power) + convert xs (power + 1)
    digitValue '0' = 0
    digitValue '1' = 1
