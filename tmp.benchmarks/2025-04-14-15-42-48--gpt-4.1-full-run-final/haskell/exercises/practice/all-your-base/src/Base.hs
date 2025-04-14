module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- | Convert a list of digits in inputBase to a list of digits in outputBase.
-- Returns an error if the input or output base is invalid, or if any digit is invalid.
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise =
        case checkDigits inputBase inputDigits of
            Just badDigit -> Left (InvalidDigit badDigit)
            Nothing ->
                let value = toDecimal inputBase inputDigits
                in if null inputDigits
                   then Right []
                   else Right (fromDecimal outputBase value)

-- | Check if all digits are valid for the given base.
-- Returns Just the first invalid digit, or Nothing if all are valid.
checkDigits :: (Ord a, Num a) => a -> [a] -> Maybe a
checkDigits base = go
  where
    go [] = Nothing
    go (d:ds)
        | d < 0 || d >= base = Just d
        | otherwise = go ds

-- | Convert a list of digits in a given base to its decimal value.
toDecimal :: Integral a => a -> [a] -> a
toDecimal base = foldl (\acc d -> acc * base + d) 0

-- | Convert a decimal value to a list of digits in the given base.
fromDecimal :: (Integral a) => a -> a -> [a]
fromDecimal _ 0 = [0]
fromDecimal base n = reverse (toDigits n)
  where
    toDigits 0 = []
    toDigits x = let (q, r) = quotRem x base in r : toDigits q
