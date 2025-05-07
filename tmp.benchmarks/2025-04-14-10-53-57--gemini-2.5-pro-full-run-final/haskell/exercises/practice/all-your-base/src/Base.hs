module Base (Error(..), rebase) where

import Data.List (foldl')
-- Removed redundant import of 'reverse'

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- | Converts a number represented as a list of digits in inputBase
-- | to a list of digits in outputBase.
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise =
        case validateDigits inputBase inputDigits of
            Just err -> Left err
            Nothing
              -- Handle empty input list explicitly as required by the test
              | null inputDigits -> Right []
              | otherwise -> Right (toDigits outputBase (fromDigits inputBase inputDigits))

-- | Validates if all digits are valid for the given base.
-- | Returns Just Error if invalid, Nothing otherwise.
validateDigits :: Integral a => a -> [a] -> Maybe (Error a)
validateDigits base = go
  where
    go [] = Nothing
    go (d:ds)
        | d < 0 || d >= base = Just (InvalidDigit d)
        | otherwise = go ds

-- | Converts a list of digits in a given base to an integer.
-- | Example: fromDigits 10 [4, 2] == 42
fromDigits :: Integral a => a -> [a] -> a
fromDigits base = foldl' (\acc digit -> acc * base + digit) 0

-- | Converts an integer to a list of digits in a given base.
-- | Example: toDigits 2 42 == [1, 0, 1, 0, 1, 0]
toDigits :: Integral a => a -> a -> [a]
toDigits _ 0 = [0] -- The number 0 is represented as [0] (unless input was [])
toDigits base n = reverse $ go n
  where
    go 0 = []
    go x = let (q, r) = x `divMod` base -- Renamed variables to q and r
           in r : go q
