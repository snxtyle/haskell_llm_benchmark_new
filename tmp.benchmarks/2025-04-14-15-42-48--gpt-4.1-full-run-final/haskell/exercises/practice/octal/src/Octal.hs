module Octal (readOct, showOct) where

-- | Convert an octal string to its decimal equivalent.
--   Invalid input is treated as octal 0.
readOct :: String -> Integer
readOct s = go s 0
  where
    go [] acc = acc
    go (c:cs) acc =
      case c of
        '0' -> step 0
        '1' -> step 1
        '2' -> step 2
        '3' -> step 3
        '4' -> step 4
        '5' -> step 5
        '6' -> step 6
        '7' -> step 7
        _   -> 0  -- invalid input, treat as 0
      where
        step d = let acc' = acc * 8 + fromIntegral d in acc' `seq` go cs acc'

-- | Show an integer as an octal string.
showOct :: Integer -> String
showOct n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = "0"
  | otherwise = reverse (toOct n)
  where
    toOct 0 = ""
    toOct x = let (q, r) = x `quotRem` 8 in intToDigit r : toOct q

    intToDigit d = case d of
      0 -> '0'
      1 -> '1'
      2 -> '2'
      3 -> '3'
      4 -> '4'
      5 -> '5'
      6 -> '6'
      7 -> '7'
      _ -> error "intToDigit: not an octal digit"
