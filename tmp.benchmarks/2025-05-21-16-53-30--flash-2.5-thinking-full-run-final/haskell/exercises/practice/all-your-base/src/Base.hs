module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits =
    -- 1. Validate input and output bases
    if inputBase < 2
        then Left InvalidInputBase
    else if outputBase < 2
        then Left InvalidOutputBase
    else
        -- 2. Convert input digits to decimal (base 10) while validating digits
        let
            -- foldl accumulates the decimal value and checks for invalid digits
            (validationResult, decimalValue) = foldl (\(err, acc) digit ->
                if err /= Right () then (err, acc) -- Propagate existing error
                else if digit < 0 || digit >= inputBase
                    then (Left (InvalidDigit digit), acc) -- Found an invalid digit
                else (Right (), acc * inputBase + digit) -- Accumulate decimal value
                ) -- Moved this ')' to align with the lambda body's indentation
                (Right (), 0) inputDigits
        in
        case validationResult of
            Left err -> Left err -- Return digit validation error
            Right () ->
                -- 3. Convert from decimal to outputBase
                if decimalValue == 0
                    then Right [0] -- Special case for zero
                else
                    let
                        -- Helper function to convert a decimal number to a list of digits in the target base
                        convertFromDecimal :: a -> a -> [a] -> [a]
                        convertFromDecimal num base acc
                            | num == 0  = acc
                            | otherwise = convertFromDecimal (num `div` base) base ((num `mod` base) : acc)
                    in
                    Right (convertFromDecimal decimalValue outputBase [])
