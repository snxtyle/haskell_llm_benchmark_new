module Palindromes (largestPalindrome, smallestPalindrome) where

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome = findBy (>)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome = findBy (<)

findBy :: (Integer -> Integer -> Bool)
       -> Integer
       -> Integer
       -> Maybe (Integer, [(Integer, Integer)])
findBy better minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = foldl step Nothing [(a, b) | a <- [minFactor .. maxFactor], b <- [a .. maxFactor]]
  where
    step :: Maybe (Integer, [(Integer, Integer)]) -> (Integer, Integer) -> Maybe (Integer, [(Integer, Integer)])
    step acc pair@(a, b) =
      let p = a * b
       in if isPalindrome p
            then case acc of
              Nothing -> Just (p, [pair])
              Just (v, fs)
                | better p v -> Just (p, [pair])
                | p == v     -> Just (v, pair : fs)
                | otherwise  -> acc
            else acc

isPalindrome :: Integer -> Bool
isPalindrome n =
  let s = show n
   in s == reverse s
