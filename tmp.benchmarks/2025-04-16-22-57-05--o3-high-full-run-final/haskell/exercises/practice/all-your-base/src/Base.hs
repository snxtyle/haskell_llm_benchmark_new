module Base (Error(..), rebase) where

import Data.List (foldl')

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- | Convert a list of digits expressed in @inputBase@ into the corresponding
-- list of digits expressed in @outputBase@.
--
-- The function performs a number of validations:
--
--   * both bases must be greater than one;
--   * the input digit list must be non‑empty (an empty list represents 0 and
--     converts to an empty list);
--   * every digit must be non‑negative and strictly smaller than @inputBase@.
--
-- In case of a validation failure the corresponding 'Error' value is returned.
-- On success the converted digit list is produced.
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase  <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | null inputDigits = Right []                       -- empty input represents 0
  | otherwise =
      case firstInvalid of
        Just d  -> Left (InvalidDigit d)
        Nothing -> Right . toDigits outputBase $ fromDigits inputBase inputDigits
  where
    -- Locate the first digit that is either negative or not representable in
    -- the given input base.
    firstInvalid = case filter invalid inputDigits of
                     []    -> Nothing
                     (d:_) -> Just d
    invalid d = d < 0 || d >= inputBase

    -- Turn a list of digits in the given base into its numeric value.
    fromDigits base = foldl' (\acc d -> acc * base + d) 0

    -- Produce the digit list of a number in the requested base.
    toDigits _    0 = [0]
    toDigits base n = reverse (build n)
      where
        build 0 = []
        build x = let (q, r) = quotRem x base
                  in r : build q
