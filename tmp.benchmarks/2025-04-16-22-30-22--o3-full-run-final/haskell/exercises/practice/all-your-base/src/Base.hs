module Base (Error(..), rebase) where

import Data.List (find)

data Error a = InvalidInputBase
             | InvalidOutputBase
             | InvalidDigit a
    deriving (Show, Eq)

-- | Convert a list of digits expressed in `inputBase`
--   to a list of digits in `outputBase`.
--   Returns an appropriate `Error` when:
--     * either base is less than, or equal to, 1
--     * any digit is negative or not strictly less than `inputBase`
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase  <= 1 = Left InvalidInputBase
    | outputBase <= 1 = Left InvalidOutputBase
    | Just d <- invalidDigit = Left (InvalidDigit d)
    | otherwise = Right result
  where
    -- First invalid digit, if any
    invalidDigit = find (\d -> d < 0 || d >= inputBase) inputDigits

    -- Numeric value of the input digit list
    value = foldl (\acc d -> acc * inputBase + d) 0 inputDigits

    -- Resulting digit list in the desired base
    result
        -- Special‑case an empty input: preserve emptiness
        | null inputDigits = []
        -- Non‑empty input that evaluates to zero should yield [0]
        | value == 0       = [0]
        | otherwise        = reverse (toDigits value)

    -- Convert a non‑zero number to digits in `outputBase`
    toDigits 0 = []
    toDigits n = let (q, r) = divMod n outputBase
                 in r : toDigits q
