module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | null inputDigits = Right []  -- Handle empty list case
    | any (\d -> d < 0 || d >= inputBase) inputDigits = 
        case findInvalidDigit inputDigits inputBase of
            Just invalidDigit -> Left $ InvalidDigit invalidDigit
            Nothing -> Left $ InvalidDigit (head inputDigits)  -- This should never happen
    | otherwise = Right $ convertToOutputBase outputBase $ convertFromInputBase inputBase inputDigits

-- Helper function to find the first invalid digit
findInvalidDigit :: Integral a => [a] -> a -> Maybe a
findInvalidDigit [] _ = Nothing
findInvalidDigit (x:xs) base
    | x < 0 || x >= base = Just x
    | otherwise = findInvalidDigit xs base

-- Convert from input base to decimal (base 10)
convertFromInputBase :: Integral a => a -> [a] -> a
convertFromInputBase base digits = sum $ zipWith (*) digits $ reverse $ map (base ^) [0..length digits - 1]

-- Convert from decimal to output base
convertToOutputBase :: Integral a => a -> a -> [a]
convertToOutputBase base n
    | n == 0 = [0]
    | otherwise = reverse $ convert n
    where
        convert 0 = []
        convert x = let (q, r) = x `divMod` base in r : convert q
