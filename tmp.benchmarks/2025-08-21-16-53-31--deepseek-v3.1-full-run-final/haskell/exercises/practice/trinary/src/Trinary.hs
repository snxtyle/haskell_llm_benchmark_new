module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri s = if all (`elem` "012") s then go 0 s else 0
  where
    go acc [] = acc
    go acc (d:ds) = let newAcc = acc * 3 + toInteger (digitValue d)
                    in newAcc `seq` go newAcc ds
    
    digitValue '0' = 0
    digitValue '1' = 1
    digitValue '2' = 2
    digitValue _   = 0  -- Guarded by the initial check

showTri :: Integer -> String
showTri n
  | n < 0     = "0"
  | n == 0    = "0"
  | otherwise = reverse $ go n
  where
    go 0 = ""
    go num = let (q, r) = num `divMod` 3
             in digitChar (fromInteger r) : go q
    
    digitChar 0 = '0'
    digitChar 1 = '1'
    digitChar 2 = '2'
    digitChar _ = '0'  -- Shouldn't happen with mod 3
