module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | null inputDigits = Right []  -- Special case for empty input
    | otherwise = 
        case validateAndConvert inputBase inputDigits of
          Left err -> Left err
          Right num -> Right (convertToBase outputBase num)
  where
    validateAndConvert :: Integral a => a -> [a] -> Either (Error a) a
    validateAndConvert base digits = go 0 digits
      where
        go n [] = Right n
        go n (d:ds)
          | d < 0 || d >= base = Left (InvalidDigit d)
          | otherwise = go (n * base + d) ds

    convertToBase :: Integral a => a -> a -> [a]
    convertToBase _ 0 = [0]  -- Simplified pattern
    convertToBase base n = reverse (go n)
      where
        go 0 = []
        go num = let (q, r) = num `divMod` base
                 in r : go q
