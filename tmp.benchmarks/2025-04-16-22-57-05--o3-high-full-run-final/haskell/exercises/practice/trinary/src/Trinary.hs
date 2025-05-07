module Trinary (readTri, showTri) where

-- | Convert a trinary number, given as a String, to its decimal value.
--   If the input contains any character other than ‘0’, ‘1’, or ‘2’
--   the function returns 0.
readTri :: Integral a => String -> a
readTri xs
  | all (`elem` validDigits) xs = go 0 xs
  | otherwise                   = 0
  where
    validDigits = "012"

    -- Strict left‑to‑right accumulation.
    go acc []     = acc
    go acc (c:cs) =
      let acc' = acc * 3 + digit c
      in acc' `seq` go acc' cs

    digit :: Num a => Char -> a
    digit '0' = 0
    digit '1' = 1
    digit '2' = 2
    digit _   = 0   -- unreachable because of the guard above

-- | Convert a non‑negative integral value to its trinary representation.
--   For 0 the representation is the single digit "0".
showTri :: Integral a => a -> String
showTri n
  | n <= 0    = "0"
  | otherwise = reverse (digits n)
  where
    digits 0 = []
    digits m =
      let (q, r) = m `quotRem` 3
      in digit r : digits q

    digit :: Integral a => a -> Char
    digit 0 = '0'
    digit 1 = '1'
    digit _ = '2'
