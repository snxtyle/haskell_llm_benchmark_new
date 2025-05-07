module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | null inputDigits = Right [] -- Handle empty input list case
    | otherwise =
        case toDecimal inputBase inputDigits of
            Left err -> Left err
            Right decimalValue -> Right (fromDecimal outputBase decimalValue)
  where
    -- Converts a list of digits in a given base to its decimal (base-10) representation.
    -- Performs validation on each digit.
    toDecimal :: Integral a => a -> [a] -> Either (Error a) a
    toDecimal base digits =
        let processDigit acc d =
                case acc of
                    Left err -> Left err -- Propagate any previous error
                    Right val ->
                        if d < 0 || d >= base
                        then Left (InvalidDigit d) -- Current digit is invalid
                        else Right (val * base + d) -- Accumulate value (Horner's method)
        in foldl processDigit (Right 0) digits

    -- Converts a decimal (base-10) number to a list of digits in a target base.
    fromDecimal :: Integral a => a -> a -> [a]
    fromDecimal _ 0 = [0] -- The number 0 is represented as a single digit 0.
    fromDecimal base n = reverse (generateDigits n)
      where
        generateDigits 0 = [] -- Base case for recursion: no more digits when number is 0.
        generateDigits x = (x `mod` base) : generateDigits (x `div` base)
