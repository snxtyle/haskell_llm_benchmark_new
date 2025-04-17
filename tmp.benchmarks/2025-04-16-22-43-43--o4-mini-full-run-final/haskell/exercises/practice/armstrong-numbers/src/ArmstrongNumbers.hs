module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong x =
  let ds = digits (abs x)
      n  = length ds
  in sum (map (^ n) ds) == x

-- Split a non-negative integer into its decimal digits
digits :: Integral a => a -> [a]
digits 0 = [0]
digits n = go n
  where
    go 0 = []
    go y = go (y `div` 10) ++ [y `mod` 10]
