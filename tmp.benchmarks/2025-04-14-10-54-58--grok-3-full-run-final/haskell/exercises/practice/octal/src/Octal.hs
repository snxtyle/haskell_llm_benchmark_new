{-# LANGUAGE BangPatterns #-}
module Octal (readOct, showOct) where

-- Convert an octal string to decimal (integer)
readOct :: String -> Integer
readOct str = go str 0
  where
    go !s !acc
      | null s = acc
      | otherwise =
          let (digit, rest) = (head s, tail s)
              val = case digit of
                      '0' -> 0
                      '1' -> 1
                      '2' -> 2
                      '3' -> 3
                      '4' -> 4
                      '5' -> 5
                      '6' -> 6
                      '7' -> 7
                      _   -> -1  -- Invalid digit
          in if val == -1
             then 0  -- Return 0 for invalid input
             else go rest $! (acc * 8) + val

-- Convert a decimal (integer) to octal string
showOct :: Integer -> String
showOct n
  | n == 0 = "0"
  | otherwise = reverse $ go n ""
  where
    go !num !acc
      | num == 0 = acc
      | otherwise =
          let (q, r) = num `divMod` 8
              digit = case r of
                        0 -> '0'
                        1 -> '1'
                        2 -> '2'
                        3 -> '3'
                        4 -> '4'
                        5 -> '5'
                        6 -> '6'
                        7 -> '7'
                        _ -> error "Impossible remainder"
          in go q $! digit : acc
