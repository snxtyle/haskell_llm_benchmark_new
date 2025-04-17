module Trinary (readTri, showTri) where

-- | Convert a trinary number, supplied as a 'String', into its decimal
--   representation (for any 'Integral' result type).  Any input that
--   contains characters other than '0', '1', or '2' is considered
--   invalid and results in 0.
readTri :: Integral a => String -> a
readTri xs
  | all isValidDigit xs = fromIntegral $ strictFoldl step 0 xs
  | otherwise           = 0
  where
    -- Accumulate the value strictly to avoid building up thunks.
    step :: Integer -> Char -> Integer
    step acc c = (acc * 3) + charToVal c

    -- Check whether a character is a valid trinary digit.
    isValidDigit :: Char -> Bool
    isValidDigit c = c == '0' || c == '1' || c == '2'

    -- Convert a trinary digit character to its numeric value.
    charToVal :: Char -> Integer
    charToVal '0' = 0
    charToVal '1' = 1
    charToVal '2' = 2
    charToVal  _  = 0  -- Will never be reached due to the guard above.

-- | Convert a decimal number (any 'Integral' type) to its trinary
--   representation.  The result is returned as a 'String'.  For 0 the
--   function returns "0".  Negative inputs are converted by prefixing a
--   minus sign to the representation of the absolute value.
showTri :: Integral a => a -> String
showTri n0 =
  let n :: Integer
      n = toInteger n0
  in case compare n 0 of
       LT -> '-' : showTri (abs n)      -- recurse with the absolute value
       EQ -> "0"
       GT -> reverse (toTriDigits n)
  where
    toTriDigits :: Integer -> String
    toTriDigits 0 = ""
    toTriDigits m =
      let (q, r) = m `quotRem` 3
          digit  = case r of
                     0 -> '0'
                     1 -> '1'
                     _ -> '2'
      in digit : toTriDigits q

-- | A strict left fold defined here so that we do not rely on
--   Data.List.foldl'.  This keeps the solution confined to Prelude.
strictFoldl :: (a -> b -> a) -> a -> [b] -> a
strictFoldl f z0 xs = go z0 xs
  where
    go acc []     = acc
    go acc (y:ys) =
      let acc' = f acc y
      in acc' `seq` go acc' ys
