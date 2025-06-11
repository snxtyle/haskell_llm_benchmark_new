module Trinary (readTri, showTri) where

-- | Convert a string that represents a trinary number (base-3) into its
-- | decimal value (in base-10).  Any string containing characters other than
-- | ‘0’, ‘1’ or ‘2’ is considered invalid and yields 0.
readTri :: String -> Integer
readTri xs
  | all isTriDigit xs = foldl' step 0 xs
  | otherwise         = 0
  where
    -- A local, strict left fold so that we don’t import Data.List.
    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' _ z []     = z
    foldl' f z (y:ys) =
      let z' = f z y
      in z' `seq` foldl' f z' ys

    -- Validate a single trinary digit.
    isTriDigit :: Char -> Bool
    isTriDigit '0' = True
    isTriDigit '1' = True
    isTriDigit '2' = True
    isTriDigit _   = False

    -- Convert a character digit to its numeric value.
    digit :: Char -> Integer
    digit '0' = 0
    digit '1' = 1
    digit '2' = 2
    digit _   = 0  -- unreachable because of the guard above

    -- Strictly accumulate the result.
    step :: Integer -> Char -> Integer
    step acc c =
      let acc' = acc * 3 + digit c
      in acc' `seq` acc'

-- | Convert a non-negative decimal number (base-10) to its trinary (base-3)
-- | string representation.  For any input ≤ 0 the function returns "0".
showTri :: Integer -> String
showTri n
  | n <= 0    = "0"
  | otherwise = reverse (build n)
  where
    build :: Integer -> String
    build 0 = []
    build m =
      let (q, r) = m `divMod` 3
      in intToDigit r : build q

    intToDigit :: Integer -> Char
    intToDigit 0 = '0'
    intToDigit 1 = '1'
    intToDigit 2 = '2'
    intToDigit _ = error "Unexpected remainder when converting to trinary."
