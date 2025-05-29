module Octal (readOct, showOct) where

readOct :: String -> Int
readOct s = go 0 s
  where
    go acc [] = acc
    go acc (d:ds)
      | d >= '0' && d <= '7' =
          let newAcc = acc * 8 + (fromEnum d - fromEnum '0')
          in newAcc `seq` go newAcc ds
      | otherwise = 0

showOct :: Integer -> String
showOct = error "You need to implement this function."
