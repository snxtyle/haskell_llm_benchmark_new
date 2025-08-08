module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)

-- modulus for the Roman alphabet
m :: Int
m = 26

-- Check if two integers are coprime
isCoprime :: Int -> Int -> Bool
isCoprime x y = gcd (x `mod` y) y == 1

-- Extended Euclidean Algorithm
-- Returns (g, x, y) such that a*x + b*y = g = gcd(a, b)
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b =
  let (g, x1, y1) = egcd (b `mod` a) a
  in (g, y1 - (b `div` a) * x1, x1)

-- Modular multiplicative inverse of a modulo n, if it exists
modInv :: Int -> Int -> Maybe Int
modInv a n =
  let a' = a `mod` n
      (g, x, _) = egcd a' n
  in if g == 1 then Just (x `mod` n) else Nothing

-- Convert a lowercase alphabetic char to an index [0..25]
toIndex :: Char -> Int
toIndex c = ord c - ord 'a'

-- Convert an index [0..25] to a lowercase alphabetic char
fromIndex :: Int -> Char
fromIndex i = chr (i + ord 'a')

-- Keep only letters and digits; lowercase letters
sanitize :: String -> String
sanitize = mapLower . filter keep
  where
    keep c = isAlpha c || isDigit c
    mapLower = map (\c -> if isAlpha c then toLower c else c)

-- Split a list into chunks of size n
groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs =
  let (g, rest) = splitAt n xs
  in g : groupN n rest

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (isCoprime a m) = Nothing
  | otherwise =
      let a' = a `mod` m
          b' = b `mod` m
          cleaned = sanitize plainText
          encChar c
            | isAlpha c =
                let i = toIndex c
                    e = (a' * i + b') `mod` m
                in fromIndex e
            | otherwise = c -- digits are unchanged
          encoded = map encChar cleaned
          grouped = unwords (groupN 5 encoded)
      in Just grouped

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  invA <- modInv a m
  let b' = b `mod` m
      cleaned = sanitize cipherText
      decChar c
        | isAlpha c =
            let y = toIndex c
                -- ensure non-negative before multiplying by invA
                z = (y - b') `mod` m
                d = (invA * z) `mod` m
            in fromIndex d
        | otherwise = c -- digits are unchanged
      decoded = map decChar cleaned
  pure decoded
