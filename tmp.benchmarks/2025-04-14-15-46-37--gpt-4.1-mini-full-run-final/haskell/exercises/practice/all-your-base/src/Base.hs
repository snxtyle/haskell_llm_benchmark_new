module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- Convert a list of digits in inputBase to an Integer
toDecimal :: Integral a => a -> [a] -> Either (Error a) Integer
toDecimal base digits
    | base < 2 = Left InvalidInputBase
    | otherwise = go digits 0
  where
    go [] acc = Right acc
    go (d:ds) acc
        | d < 0 || d >= base = Left (InvalidDigit d)
        | otherwise = go ds (acc * toInteger base + toInteger d)

-- Convert an Integer to a list of digits in outputBase
fromDecimal :: Integral a => a -> Integer -> Either (Error a) [a]
fromDecimal base n
    | base < 2 = Left InvalidOutputBase
    | n == 0 = Right []
    | n < 0 = Left InvalidOutputBase -- negative numbers not supported
    | otherwise = Right (reverse (go n))
  where
    go 0 = []
    go x = let (q, r) = x `divMod` toInteger base in fromIntegral r : go q

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | null inputDigits = Right []
    | otherwise = do
        decimalValue <- toDecimal inputBase inputDigits
        fromDecimal outputBase decimalValue
