module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise = 
      let n' = toInteger n
          s = show n'
          len = length s
          digits = map (\c -> toInteger (fromEnum c - fromEnum '0')) s
          total = sum $ map (^ len) digits
      in n' == total
