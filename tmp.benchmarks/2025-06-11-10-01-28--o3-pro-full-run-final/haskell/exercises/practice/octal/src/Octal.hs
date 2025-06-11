{-# LANGUAGE Safe #-}

-- | Octal (base-8) utilities implemented from first principles.
--
--   No external numeric-conversion helpers are used.
module Octal
  ( readOct   -- ^ Convert an octal string to a decimal number
  , showOct   -- ^ Render an integral number in octal notation
  ) where


-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Convert an octal number, supplied as a String, to its decimal value.
--
--   * Only the characters ‘0’‥‘7’ are considered valid.
--   * Any other character (or an empty string) makes the whole input invalid,
--     in which case the function returns @0@.
readOct :: String -> Integer
readOct str =
  case parseDigits str 0 of
    Just n  -> n
    Nothing -> 0
  where
    -- Strict left-to-right parser with an accumulating parameter.
    parseDigits :: String -> Integer -> Maybe Integer
    parseDigits []     acc = Just acc
    parseDigits (c:cs) acc =
      case charToDigit c of
        Nothing -> Nothing
        Just d  ->
          let acc' = acc * 8 + d
          in  acc' `seq` parseDigits cs acc'

-- | Render an integral value as an octal string.
--
--   • For @0@ the string \"0\" is returned.  
--   • Negative numbers are prefixed with “-”.
showOct :: (Integral a) => a -> String
showOct n
  | n == 0    = "0"
  | n < 0     = '-' : showOct (abs n)
  | otherwise = reverse (digits n)
  where
    digits :: (Integral a) => a -> String
    digits 0 = ""
    digits x =
      let (q, r) = quotRem x 8
          ch     = toEnum (fromIntegral r + fromEnum '0') :: Char
      in  ch : digits q


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Convert a single octal digit character to its numeric value.
--   Returns Nothing when the character is not in ‘0’..‘7’.
charToDigit :: Char -> Maybe Integer
charToDigit c
  | c >= '0' && c <= '7' = Just . toInteger $ fromEnum c - fromEnum '0'
  | otherwise            = Nothing
