module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise =
      let ds = digits n
          k  = length ds
      in sum (map (^ k) ds) == n

digits :: Integral a => a -> [a]
digits 0 = [0]
digits x = reverse (go x)
  where
    go 0 = []
    go y =
      let (q, r) = y `divMod` 10
      in r : go q
