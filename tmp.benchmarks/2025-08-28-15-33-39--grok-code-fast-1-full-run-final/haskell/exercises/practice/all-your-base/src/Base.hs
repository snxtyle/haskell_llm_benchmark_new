module Base (Error(..), rebase) where

import Data.List (unfoldr)

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise = case filter (\d -> d < 0 || d >= inputBase) inputDigits of
      (invalid:_) -> Left (InvalidDigit invalid)
      [] -> if null inputDigits
            then Right []
            else Right (toDigits outputBase (fromDigits inputBase inputDigits))

fromDigits :: Integral a => a -> [a] -> a
fromDigits base = foldl (\acc d -> acc * base + d) 0

toDigits :: Integral a => a -> a -> [a]
toDigits _ 0 = [0]
toDigits base n = reverse (unfoldr go n)
  where go x | x == 0 = Nothing
             | otherwise = Just (x `mod` base, x `div` base)
