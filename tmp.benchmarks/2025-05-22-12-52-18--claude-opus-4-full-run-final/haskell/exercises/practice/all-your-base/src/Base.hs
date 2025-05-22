module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | null inputDigits = Right []  -- Handle empty input
    | otherwise = case validateDigits inputBase inputDigits of
        Just invalidDigit -> Left (InvalidDigit invalidDigit)
        Nothing -> Right (toBase outputBase (fromBase inputBase inputDigits))

-- Validate that all digits are valid for the given base
validateDigits :: Integral a => a -> [a] -> Maybe a
validateDigits base digits = 
    case filter (\d -> d < 0 || d >= base) digits of
        (invalid:_) -> Just invalid
        [] -> Nothing

-- Convert from a given base to decimal
fromBase :: Integral a => a -> [a] -> a
fromBase base digits = foldl (\acc digit -> acc * base + digit) 0 digits

-- Convert from decimal to a given base
toBase :: Integral a => a -> a -> [a]
toBase base 0 = [0]
toBase base num = reverse (toBaseHelper base num)
  where
    toBaseHelper _ 0 = []
    toBaseHelper b n = (n `mod` b) : toBaseHelper b (n `div` b)
