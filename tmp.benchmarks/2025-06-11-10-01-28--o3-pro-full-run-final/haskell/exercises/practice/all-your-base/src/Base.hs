module Base (Error(..), rebase) where

-- | Possible errors that can arise during conversion.
data Error a
    = InvalidInputBase      -- ^ The input base must be ≥ 2
    | InvalidOutputBase     -- ^ The output base must be ≥ 2
    | InvalidDigit a        -- ^ A digit supplied is not valid for the input base
    deriving (Show, Eq)

-- | Convert a list of digits from one base to another.
--
--   The digits are given most–significant first.
--
--   Error conditions:
--     * input base  < 2  ➜  'InvalidInputBase'
--     * output base < 2  ➜  'InvalidOutputBase'
--     * a digit < 0 or ≥ input base ➜ 'InvalidDigit' offendingDigit
--     * an empty input list ➜ 'InvalidDigit 0'
--
--   Leading zeros are allowed; a value of zero is represented as [0].
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2  = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | null inputDigits = Left (InvalidDigit 0)
  | otherwise =
      case firstInvalidDigit of
        Just d  -> Left (InvalidDigit d)
        Nothing -> Right (toDigits convertedValue)
  where
    -- Detect the first digit that is negative or too large for the input base.
    firstInvalidDigit = go inputDigits
      where
        go []     = Nothing
        go (d:ds)
          | d < 0 || d >= inputBase = Just d
          | otherwise               = go ds

    -- Convert the input digit list to an Integer (base-10) value.
    convertedValue :: Integer
    convertedValue = foldl step 0 inputDigits
      where
        b = toInteger inputBase
        step acc d = acc * b + toInteger d

    -- Convert an Integer value to the desired base, yielding [a].
    toDigits :: Integral a => Integer -> [a]
    toDigits 0 = [0]
    toDigits n = reverse (build n)
      where
        b = toInteger outputBase
        build 0 = []
        build x = let (q, r) = x `divMod` b
                  in fromIntegral r : build q
