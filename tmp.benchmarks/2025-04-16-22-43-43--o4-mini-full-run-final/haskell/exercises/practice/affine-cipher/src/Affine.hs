module Affine (decode, encode) where

import Data.Char (isAlphaNum, isDigit, toLower, chr, ord)

-- | Encrypt the given plaintext with key (a,b).
--   Returns Nothing if gcd(a,26) ≠ 1.
encode :: (Int, Int) -> String -> Maybe String
encode (a,b) text
  | gcd a m /= 1 = Nothing
  | otherwise    = Just . unwords . chunk 5 $ map encChar clean
  where
    m      = 26
    -- normalize: lowercase, drop non-alphanum, digits kept
    clean  = [ c | c' <- text
                 , let c = toLower c'
                 , isAlphaNum c
                 ]
    encChar c
      | isDigit c = c
      | otherwise =
          let x = ord c - ord 'a'
              y = (a * x + b) `mod` m
          in chr (ord 'a' + y)

-- | Decrypt the given ciphertext with key (a,b).
--   Returns Nothing if gcd(a,26) ≠ 1 (i.e. no modular inverse).
decode :: (Int, Int) -> String -> Maybe String
decode (a,b) cipher
  | gcd a m /= 1 = Nothing
  | otherwise    = Just $ map decChar clean
  where
    m      = 26
    inv    = modularInverse a m
    clean  = [ c | c' <- cipher
                 , let c = toLower c'
                 , isAlphaNum c
                 ]
    decChar c
      | isDigit c = c
      | otherwise =
          let y = ord c - ord 'a'
              x = (inv * (y - b)) `mod` m
          in chr (ord 'a' + x)

-- | Find the modular multiplicative inverse of a mod m (assumes gcd(a,m)==1).
modularInverse :: Int -> Int -> Int
modularInverse a m = head [ i | i <- [1..m], (a * i) `mod` m == 1 ]

-- | Split a list into chunks of the given size.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (h,t) = splitAt n xs
             in h : chunk n t
