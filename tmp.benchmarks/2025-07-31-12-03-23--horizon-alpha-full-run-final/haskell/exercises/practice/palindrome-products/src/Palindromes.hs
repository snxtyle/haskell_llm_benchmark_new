module Palindromes (largestPalindrome, smallestPalindrome) where

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise =
      let prodsDesc = [p | p <- [maxFactor * maxFactor, (maxFactor * maxFactor - 1) .. minFactor * minFactor], isPalindrome p]
      in findFirstWithPairsDescending prodsDesc minFactor maxFactor

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise =
      let prodsAsc = [p | p <- [minFactor * minFactor .. maxFactor * maxFactor], isPalindrome p]
      in findFirstWithPairsAscending prodsAsc minFactor maxFactor

-- Helpers

isPalindrome :: Integer -> Bool
isPalindrome n =
  let s = show (abs n)
  in s == reverse s

-- Collect all factor pairs (a, b) such that:
-- minF <= a <= b <= maxF, and a * b == p
factorPairsInRange :: Integer -> Integer -> Integer -> [(Integer, Integer)]
factorPairsInRange p minF maxF =
  [ (a, b)
  | a <- [minF .. maxF]
  , p `mod` a == 0
  , let b = p `div` a
  , a <= b
  , b >= minF
  , b <= maxF
  , a * b == p
  ]

findFirstWithPairsAscending :: [Integer] -> Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
findFirstWithPairsAscending [] _ _ = Nothing
findFirstWithPairsAscending (p:ps) minF maxF =
  let pairs = factorPairsInRange p minF maxF
  in if null(pairs) then findFirstWithPairsAscending ps minF maxF
     else Just (p, pairs)

findFirstWithPairsDescending :: [Integer] -> Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
findFirstWithPairsDescending [] _ _ = Nothing
findFirstWithPairsDescending (p:ps) minF maxF =
  let pairs = factorPairsInRange p minF maxF
  in if null(pairs) then findFirstWithPairsDescending ps minF maxF
     else Just (p, pairs)
