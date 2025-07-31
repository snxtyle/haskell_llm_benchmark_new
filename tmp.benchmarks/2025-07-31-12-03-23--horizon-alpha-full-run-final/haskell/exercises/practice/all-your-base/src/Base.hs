module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- Convert digits from inputBase to outputBase with validation.
-- Rules:
-- - Bases must be >= 2, otherwise error.
-- - All digits must be in [0 .. inputBase-1], otherwise InvalidDigit digit.
-- - An empty input should return [].
-- - An input consisting only of zeros represents zero and returns [0].
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2  = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | null inputDigits = Right []
  | otherwise = do
      -- Validate digits
      mapM_ validateDigit inputDigits
      let digitsStripped = dropWhile (== 0) inputDigits
      if null digitsStripped
        then Right [0]
        else
          let value = fromDigits inputBase digitsStripped
          in Right (toDigits outputBase value)
  where
    validateDigit d
      | d < 0 || d >= inputBase = Left (InvalidDigit d)
      | otherwise               = Right ()

    -- Combine digits interpreted in the given base to a single number.
    -- Assumes digits are valid and without leading zeros.
    fromDigits :: Integral a => a -> [a] -> a
    fromDigits b = foldl (\acc d -> acc * b + d) 0

    -- Produce digits in the given base from a non-negative value.
    -- Assumes base >= 2 and value > 0.
    toDigits :: Integral a => a -> a -> [a]
    toDigits b n = reverse (build n)
      where
        build 0 = []
        build x =
          let (q, r) = x `quotRem` b
          in r : if q == 0 then [] else build q
