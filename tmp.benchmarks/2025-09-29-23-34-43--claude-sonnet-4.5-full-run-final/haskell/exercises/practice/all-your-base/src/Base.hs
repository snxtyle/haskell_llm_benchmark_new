module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | null inputDigits = Right []
    | otherwise = do
        -- Validate all digits are in valid range for input base
        validateDigits inputDigits
        -- Convert from input base to base 10
        let base10 = toBase10 inputBase inputDigits
        -- Convert from base 10 to output base
        Right (fromBase10 outputBase base10)
  where
    validateDigits [] = Right ()
    validateDigits (d:ds)
        | d < 0 || d >= inputBase = Left (InvalidDigit d)
        | otherwise = validateDigits ds
    
    -- Convert from input base to base 10
    toBase10 :: Integral a => a -> [a] -> a
    toBase10 _ [] = 0
    toBase10 base digits = foldl (\acc digit -> acc * base + digit) 0 digits
    
    -- Convert from base 10 to output base
    fromBase10 :: Integral a => a -> a -> [a]
    fromBase10 _ 0 = [0]
    fromBase10 base n = reverse (go n)
      where
        go 0 = []
        go x = (x `mod` base) : go (x `div` base)
