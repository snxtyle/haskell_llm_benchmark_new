module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase <= 1    = Left InvalidInputBase
    | outputBase <= 1   = Left InvalidOutputBase
    | any invalidDigit inputDigits = Left (InvalidDigit (head (filter invalidDigit inputDigits)))
    | otherwise         = Right (toBase outputBase (fromBase inputBase inputDigits))
  where
    invalidDigit d = d < 0 || d >= inputBase

    -- Convert from given base to decimal (base 10)
    fromBase :: Integral a => a -> [a] -> a
    fromBase base digits = foldl (\acc d -> acc * base + d) 0 digits

    -- Convert from decimal (base 10) to given base
    toBase :: Integral a => a -> a -> [a]
    toBase base n
        | n == 0    = []
        | otherwise = toBase base (n `div` base) ++ [n `mod` base]
