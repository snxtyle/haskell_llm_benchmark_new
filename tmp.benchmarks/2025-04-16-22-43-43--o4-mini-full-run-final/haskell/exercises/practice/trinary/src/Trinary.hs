{-# LANGUAGE BangPatterns #-}

module Trinary (readTri, showTri) where

-- | Convert a base‑3 string to its decimal value.
-- | Invalid strings (containing any non‑0,1,2) produce 0.
readTri :: Integral a => String -> a
readTri s
  | all isValid s = go 0 s
  | otherwise     = 0
  where
    isValid c = c == '0' || c == '1' || c == '2'

    go :: Integral a => a -> String -> a
    go !acc []     = acc
    go !acc (c:cs) = go (acc * 3 + digitToInt c) cs

    digitToInt :: Integral a => Char -> a
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt '2' = 2
    digitToInt _   = 0  -- unreachable thanks to isValid

-- | Convert a non‑negative decimal to its base‑3 string.
-- | 0 maps to "0"; negative inputs error out.
showTri :: Integral a => a -> String
showTri n
  | n < 0     = error "Negative input"
  | n == 0    = "0"
  | otherwise = build n
  where
    build :: Integral a => a -> String
    build 0 = ""
    build x =
      let (q, r) = x `divMod` 3
      in build q ++ [intToDigit r]

    intToDigit :: Integral a => a -> Char
    intToDigit d
      | d == 0    = '0'
      | d == 1    = '1'
      | d == 2    = '2'
      | otherwise = '0'  -- unreachable
