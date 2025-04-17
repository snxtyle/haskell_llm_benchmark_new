{-# LANGUAGE BangPatterns #-}

module Octal (readOct, showOct) where

--------------------------------------------------------------------
-- Conversion from octal (supplied as a String) to a generic
-- Integral type.  Invalid input yields 0, as required.
--------------------------------------------------------------------
-- | Convert a textual representation of an octal number into its decimal
--   equivalent.  If the supplied text is not a valid octal literal the
--   function returns 0, as per the exercise specification.
--
--   Examples:
--
--   >>> readOct "233" :: Integer
--   155
--
--   >>> readOct "9abc" :: Int
--   0
readOct :: Integral a => String -> a
readOct str
  | null str                 = 0
  | not (all isOctDigit str) = 0
  | otherwise                = fromInteger (go 0 str)
  where
    -- strict left‑fold to avoid building thunks
    go :: Integer -> String -> Integer
    go !acc []     = acc
    go !acc (c:cs) = go (acc * 8 + digitValue c) cs

    isOctDigit c = c >= '0' && c <= '7'
    digitValue c = toInteger (fromEnum c - fromEnum '0')

--------------------------------------------------------------------
-- Conversion from a generic Integral value to its octal textual
-- representation.
--------------------------------------------------------------------
-- | Convert a decimal integer into its octal string representation.
--
--   Negative inputs are rendered with a leading ‘‑’.
--
--   Examples:
--
--   >>> showOct (155 :: Integer)
--   "233"
--
--   >>> showOct (0 :: Int)
--   "0"
showOct :: Integral a => a -> String
showOct n
  | n == 0    = "0"
  | n < 0     = '-' : showOct (abs n)
  | otherwise = reverse (digits n)
  where
    digits :: Integral a => a -> String
    digits 0 = []
    digits x =
      let (q, r) = x `quotRem` 8
          char   = toEnum (fromIntegral r + fromEnum '0') :: Char
      in char : digits q
