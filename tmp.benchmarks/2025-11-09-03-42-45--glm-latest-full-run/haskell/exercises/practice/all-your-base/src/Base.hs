module Base (Error(..), rebase) where

import Data.List (find)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    -- Handle the special case of an empty input list
    | null inputDigits = Right []
    -- 1. Validate bases
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    -- 2. Validate digits
    | otherwise =
        case find (\d -> d < 0 || d >= inputBase) inputDigits of
            Just invalidDigit -> Left (InvalidDigit invalidDigit)
            -- 3. Perform conversion
            Nothing ->
                let -- Convert from inputBase to an integer value
                    decimalValue = foldl (\acc d -> acc * inputBase + d) 0 inputDigits
                    -- Convert from integer value to outputBase
                    toOutputBase 0 = [0]
                    toOutputBase n = reverse (go n)
                      where
                        go 0 = []
                        go x = (x `mod` outputBase) : go (x `div` outputBase)
                in Right (toOutputBase decimalValue)
