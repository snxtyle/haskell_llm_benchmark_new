module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- Convert a list of digits from a given base into a decimal number.
-- e.g., toDecimal 2 [1,0,1] == 5
toDecimal :: Integral a => a -> [a] -> a
toDecimal base = foldl (\acc digit -> acc * base + digit) 0

-- Convert a decimal number into a list of digits in a given base.
-- e.g., fromDecimal 2 5 == [1,0,1]
fromDecimal :: Integral a => a -> a -> [a]
fromDecimal _ 0 = [0]
fromDecimal base n = reverse $ go n
  where
    go 0 = []
    go x = r : go q
      where (q, r) = x `divMod` base

-- Validate that all digits are valid for the given base.
-- A digit d is valid if 0 <= d < base.
validateDigits :: Integral a => a -> [a] -> Either (Error a) ()
validateDigits _ [] = Right ()
validateDigits base (d:ds)
  | d >= 0 && d < base = validateDigits base ds
  | otherwise          = Left (InvalidDigit d)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | null inputDigits = Right []
  | otherwise = do
      validateDigits inputBase inputDigits
      let decimalValue = toDecimal inputBase inputDigits
      return $ fromDecimal outputBase decimalValue
