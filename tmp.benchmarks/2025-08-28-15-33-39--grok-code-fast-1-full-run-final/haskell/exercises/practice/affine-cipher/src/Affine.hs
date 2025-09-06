module Affine (decode, encode) where

import Data.Char (isAlphaNum, isLetter, ord, chr, toLower)
import Data.List (unfoldr)

m :: Int
m = 26

isCoprime :: Int -> Bool
isCoprime a = gcd a m == 1

extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b = let (g, x', y') = extendedGcd b (a `rem` b)
                  in (g, y', x' - (a `quot` b) * y')

modInverse :: Int -> Int -> Maybe Int
modInverse a m' = let (g, x, _) = extendedGcd a m'
                  in if g == 1 then Just ((x `mod` m' + m') `mod` m') else Nothing

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (isCoprime a) = Nothing
  | otherwise = Just $ group5 $ map encryptChar $ filter isAlphaNum $ map toLower plainText
  where
    encryptChar c
      | isLetter c = let i = ord c - ord 'a'
                     in chr ((a * i + b) `mod` m + ord 'a')
      | otherwise = c
    group5 s = unwords $ unfoldr go s
      where go [] = Nothing
            go xs = Just (take 5 xs, drop 5 xs)

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (isCoprime a) = Nothing
  | otherwise = case modInverse a m of
      Nothing -> Nothing
      Just inv -> Just $ map decryptChar $ filter isAlphaNum $ map toLower $ filter (/= ' ') cipherText
        where
          decryptChar c
            | isLetter c = let y = ord c - ord 'a'
                           in chr ((inv * ((y - b) `mod` m)) `mod` m + ord 'a')
            | otherwise = c
