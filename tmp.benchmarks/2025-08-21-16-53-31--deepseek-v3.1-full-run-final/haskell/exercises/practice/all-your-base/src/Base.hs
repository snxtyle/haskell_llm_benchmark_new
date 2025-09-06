module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | any (\d -> d < 0 || d >= inputBase) inputDigits = 
        case filter (\d -> d < 0 || d >= inputBase) inputDigits of
            (x:_) -> Left (InvalidDigit x)
            [] -> Left InvalidInputBase  -- Shouldn't happen due to the guard
    | null inputDigits = Right [0]
    | otherwise = 
        let decimalValue = convertFromBase inputBase inputDigits
        in if decimalValue == 0
            then Right [0]
            else Right (convertToBase outputBase decimalValue)

-- Convert from given base to decimal
convertFromBase :: Integral a => a -> [a] -> a
convertFromBase base digits = 
    foldl (\acc d -> acc * base + d) 0 digits

-- Convert from decimal to given base
convertToBase :: Integral a => a -> a -> [a]
convertToBase _ 0 = [0]
convertToBase base n = convertToBase' base n []
    where
        convertToBase' _ 0 acc = acc
        convertToBase' base' n' acc = 
            let (quotient, remainder) = n' `divMod` base'
            in convertToBase' base' quotient (remainder:acc)
