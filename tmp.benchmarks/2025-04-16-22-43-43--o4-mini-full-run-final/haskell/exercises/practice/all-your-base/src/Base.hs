module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2                 = Left InvalidInputBase
    | outputBase < 2                = Left InvalidOutputBase
    | null inputDigits              = Right []
    | inputDigits == [0]            = Right []
    | not (all validDigit inputDigits) = Left (InvalidDigit badDigit)
    | otherwise =
        let value = toIntegerValue inputBase inputDigits
        in if value == 0
           then Right []
           else Right (convert outputBase value)
  where
    validDigit d = d >= 0 && d < inputBase
    badDigit     = head [d | d <- inputDigits, not (validDigit d)]

    -- compute the numeric value in an Integer to avoid overflow in intermediate steps
    toIntegerValue :: Integral a => a -> [a] -> Integer
    toIntegerValue base = foldl step 0
      where
        step acc d = acc * fromIntegral base + fromIntegral d

    -- convert a positive Integer to digits in the given base
    convert :: Integral a => a -> Integer -> [a]
    convert _     0 = []  -- unreachable when value == 0 is handled above
    convert base n = map fromInteger (go n)
      where
        b     = toInteger base
        go 0 = []
        go x = let (q, r) = x `divMod` b in go q ++ [r]
