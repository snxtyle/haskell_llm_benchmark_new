module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (map (^ numDigits) ds)
  where
    ds = digits (abs n)
    numDigits = length ds

digits :: Integral a => a -> [a]
digits n
  | n < 10 = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]
