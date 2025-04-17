module Octal (readOct, showOct) where

-- | Convert an octal number supplied as a string to its decimal value.
--   If the input contains any character other than the octal digits 0‑7,
--   the result is defined to be 0.
--
--   The result type is polymorphic over every ‘Integral’ type so it can be
--   used with ‘Int’, ‘Integer’, etc.
readOct :: Integral a => String -> a
readOct str =
  case go 0 str of
    Nothing -> 0                    -- invalid character ⇒ 0
    Just n  -> fromInteger n
  where
    -- Use ‘Integer’ for the accumulator to avoid overflow while scanning.
    go :: Integer -> String -> Maybe Integer
    go acc [] = Just acc
    go _   (c : _) | c < '0' || c > '7' = Nothing
    go acc (c : cs) =
      let acc' = acc * 8 + toInteger (digitValue c)
      in  acc' `seq` go acc' cs

    digitValue :: Char -> Int
    digitValue ch = fromEnum ch - fromEnum '0'

-- | Convert a decimal number to its octal string representation.
--   Negative inputs are prefixed with “-”.
showOct :: Integral a => a -> String
showOct n
  | n < 0     = '-' : toOct (abs n)
  | n == 0    = "0"
  | otherwise = toOct n
  where
    toOct :: Integral a => a -> String
    toOct x = reverse (build x)

    build :: Integral a => a -> String
    build 0 = []
    build y =
      let (q, r) = quotRem y 8
          ch     = toEnum (fromIntegral r + fromEnum '0') :: Char
      in  ch : build q
