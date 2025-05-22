module Palindromes (largestPalindrome, smallestPalindrome) where

-- Check if a number is a palindrome
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- Generate all products and their factor pairs within the range
generateProducts :: Integer -> Integer -> [(Integer, (Integer, Integer))]
generateProducts minFactor maxFactor = 
  [(i * j, (i, j)) | i <- [minFactor..maxFactor], j <- [i..maxFactor]]

-- Find all palindromic products and group by value
findPalindromes :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
findPalindromes minFactor maxFactor = 
  let products = generateProducts minFactor maxFactor
      palindromes = filter (isPalindrome . fst) products
      grouped = groupByValue palindromes
  in grouped
  where
    groupByValue :: [(Integer, (Integer, Integer))] -> [(Integer, [(Integer, Integer)])]
    groupByValue [] = []
    groupByValue xs = 
      let sortedXs = map (\(val, pair) -> (val, pair)) xs
          groupedList = foldr insertOrGroup [] sortedXs
      in map (\(val, pairs) -> (val, pairs)) groupedList
    
    insertOrGroup :: (Integer, (Integer, Integer)) -> [(Integer, [(Integer, Integer)])] -> [(Integer, [(Integer, Integer)])]
    insertOrGroup (val, pair) [] = [(val, [pair])]
    insertOrGroup (val, pair) ((v, pairs):rest)
      | val == v = (v, pair:pairs) : rest
      | otherwise = (v, pairs) : insertOrGroup (val, pair) rest

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = 
      let palindromes = findPalindromes minFactor maxFactor
      in case palindromes of
           [] -> Nothing
           _ -> let maxPalindrome = maximum (map fst palindromes)
                    factors = concatMap snd (filter ((== maxPalindrome) . fst) palindromes)
                in Just (maxPalindrome, factors)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | otherwise = 
      let palindromes = findPalindromes minFactor maxFactor
      in case palindromes of
           [] -> Nothing
           _ -> let minPalindrome = minimum (map fst palindromes)
                    factors = concatMap snd (filter ((== minPalindrome) . fst) palindromes)
                in Just (minPalindrome, factors)
