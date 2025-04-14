module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri s =
  if all (\c -> c == '0' || c == '1' || c == '2') s
  then fst $ foldl (\(acc, power) c ->
                      let digit = fromEnum c - fromEnum '0'
                          newAcc = acc + fromIntegral digit * power  -- Changed fromInteger to fromIntegral
                          newPower = power * 3
                      in newAcc `seq` newPower `seq` (newAcc, newPower)
                   ) (0, 1) (reverse s)
  else 0

showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse $ go n []
  where
    go 0 acc = acc
    go n acc =
      let rem = n `mod` 3
          digit = case rem of
                    0 -> '0'
                    1 -> '1'
                    2 -> '2'
      in go (n `div` 3) (digit : acc)
