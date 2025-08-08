module Affine (decode, encode) where

import Data.Char (chr, ord, toLower, isDigit)

-- The size of the Roman alphabet
m :: Int
m = 26

-- ASCII letter predicate
isAsciiLetter :: Char -> Bool
isAsciiLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

-- Convert a lowercase letter to its index [0..25]
toIndex :: Char -> Int
toIndex c = ord (toLower c) - ord 'a'

-- Convert an index [0..25] back to a lowercase letter
fromIndex :: Int -> Char
fromIndex n = chr (n + ord 'a')

-- Modular multiplicative inverse of a modulo modulus, if it exists
modInv :: Int -> Int -> Maybe Int
modInv a modulus
  | gcd a modulus /= 1 = Nothing
  | otherwise =
      let a' = a `mod` modulus
          invs = [x | x <- [0 .. modulus - 1], (a' * x) `mod` modulus == 1]
      in case invs of
           (x:_) -> Just x
           []    -> Nothing

-- Chunk a list into sublists of length n
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = error "chunksOf: n must be positive"
  | null xs   = []
  | otherwise = let (h, t) = splitAt n xs
                in h : chunksOf n t

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | gcd a m /= 1 = Nothing
  | otherwise    =
      let a' = a `mod` m
          b' = b `mod` m
          -- Keep only ASCII letters and digits; normalize letters to lowercase
          normalized = [ if isAsciiLetter c then toLower c else c
                       | c <- plainText
                       , isAsciiLetter c || isDigit c
                       ]
          -- Encrypt letters; leave digits unchanged
          encrypted = [ if isDigit c
                        then c
                        else
                          let i = toIndex c
                              e = (a' * i + b') `mod` m
                          in fromIndex e
                      | c <- normalized
                      ]
      in Just (unwords (chunksOf 5 encrypted))

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText =
  case modInv a m of
    Nothing   -> Nothing
    Just inv  ->
      let b' = b `mod` m
          -- Keep only ASCII letters and digits; normalize letters to lowercase
          normalized = [ if isAsciiLetter c then toLower c else c
                       | c <- cipherText
                       , isAsciiLetter c || isDigit c
                       ]
          -- Decrypt letters; leave digits unchanged
          decrypted = [ if isDigit c
                        then c
                        else
                          let y = toIndex c
                              i = (inv * (y - b')) `mod` m
                          in fromIndex i
                      | c <- normalized
                      ]
      in Just decrypted
