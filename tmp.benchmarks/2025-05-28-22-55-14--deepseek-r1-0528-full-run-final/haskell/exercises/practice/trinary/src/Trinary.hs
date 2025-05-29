module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri s = go s 0
  where
    go [] acc = acc
    go (d:ds) acc =
      case d of
        '0' -> go ds $! (acc*3)
        '1' -> go ds $! (acc*3+1)
        '2' -> go ds $! (acc*3+2)
        _   -> 0

showTri :: Integer -> String
showTri n
  | n < 0 = "0"
  | n == 0 = "0"
  | otherwise = go n ""
  where
    go 0 acc = acc
    go num acc = 
      let (q, r) = num `divMod` 3
      in go q (toChar r : acc)
    toChar r = 
      case r of
        0 -> '0'
        1 -> '1'
        2 -> '2'
        _ -> error "Unexpected remainder in toChar"
