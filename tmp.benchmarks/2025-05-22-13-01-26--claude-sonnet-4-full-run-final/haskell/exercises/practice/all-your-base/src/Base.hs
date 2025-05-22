module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | any (\d -> d < 0 || d >= inputBase) inputDigits = 
        Left (InvalidDigit (head (filter (\d -> d < 0 || d >= inputBase) inputDigits)))
    | null inputDigits = Right []
    | otherwise = Right (toBase outputBase (fromBase inputBase inputDigits))

-- Convert from given base to decimal
fromBase :: Integral a => a -> [a] -> a
fromBase base digits = foldl (\acc digit -> acc * base + digit) 0 digits

-- Convert from decimal to given base
toBase :: Integral a => a -> a -> [a]
toBase _ 0 = []
toBase base n = reverse (toBase' base n)
  where
    toBase' _ 0 = []
    toBase' b x = (x `mod` b) : toBase' b (x `div` b)
