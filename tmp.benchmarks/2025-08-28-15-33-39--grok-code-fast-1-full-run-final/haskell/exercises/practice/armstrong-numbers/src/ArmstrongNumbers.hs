module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0 = False
  | otherwise = let ds = digits n
                    len = length ds
                in sum [d ^ len | d <- ds] == n
  where digits 0 = [0]
        digits m = go m []
          where go 0 acc = acc
                go x acc = go (x `div` 10) (x `mod` 10 : acc)
