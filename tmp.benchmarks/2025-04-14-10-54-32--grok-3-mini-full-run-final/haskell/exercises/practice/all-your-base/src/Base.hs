module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | not (null invalidDigits) = Left (InvalidDigit (head invalidDigits))
    | otherwise = Right (convertToBase outputBase decimal)
  where
    invalidDigits = [d | d <- inputDigits, d < 0 || d >= inputBase]
    decimal = sum [digit * (inputBase ^ i) | (i, digit) <- zip [0..] (reverse inputDigits)]
    
    convertToBase :: Integral a => a -> a -> [a]
    convertToBase base n
        | n == 0 = [0]
        | otherwise = reverse (go n [])
      where
        go 0 acc = acc
        go num acc = go (num `div` base) ((num `mod` base) : acc)
