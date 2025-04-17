module Alphametics (solve) where

import Data.Char (isAlpha)
import Data.List (nub, delete, sortOn)
import Data.Maybe (fromJust, listToMaybe)

-- | Solve an alphametics puzzle.
--
--   The puzzle has to be a sum of words that equals another word, e.g.
--
--   > "SEND + MORE = MONEY"
--
--   or (as used by some tracks)
--
--   > "SEND + MORE == MONEY"
--
--   The function returns the first solution it finds as an association list
--   mapping every letter that occurs in the puzzle to the digit that replaces
--   it.  If the puzzle is unsolvable, it returns 'Nothing'.
--
--   The returned list is sorted alphabetically by the letters so that the
--   order is stable and predictable for the tests.
solve :: String -> Maybe [(Char, Int)]
solve puzzle
  | length letters > 10 = Nothing          -- we only have the digits 0‑9
  | otherwise           = fmap (sortOn fst) $ listToMaybe solutions
  where
    -- Strip all white‑space characters.
    compact       = filter (/= ' ') $ map replaceEq puzzle

    -- Keep '=' characters as they are, everything else unchanged.
    replaceEq :: Char -> Char
    replaceEq '=' = '='
    replaceEq c   = c

    -- Split the compact form into left‑hand side (addends) and right‑hand side
    -- (result word).
    (lhs, rhs')   = break (== '=') compact
    rhs           = dropWhile (== '=') rhs'      -- drop one or two '='
    addends       = splitOn '+' lhs
    resultWord    = rhs

    wordsInPuzzle = addends ++ [resultWord]

    -- All distinct letters as they appear from left to right.
    letters       = nub $ filter isAlpha compact

    -- Leading letters of multi‑letter words must not be mapped to 0.
    leading       = nub [head w | w <- wordsInPuzzle, length w > 1]

    -- All possible assignments of digits to letters that respect the
    -- non‑leading‑zero constraint, produced as a lazy list.
    solutions     =
      [ ass
      | digits <- permutationsN (length letters) [0 .. 9]
      , let ass = zip letters digits
      , all (\(c, d) -> c `notElem` leading || d /= 0) ass
      , validAssignment ass
      ]

    -- Evaluate all addends and the result under a particular assignment.
    validAssignment ass =
      sum (map (wordValue ass) addends) == wordValue ass resultWord

-- | Compute the numeric value of a word under an assignment.
wordValue :: [(Char, Int)] -> String -> Int
wordValue ass = foldl (\acc c -> acc * 10 + digit c) 0
  where
    digit c = fromJust (lookup c ass)

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn d s  = foldr step [""] s
  where
    step c (x : xs)
      | c == d    = "" : x : xs
      | otherwise = (c : x) : xs
    step _ _ = error "unreachable"

-- | All permutations of length n drawn from the list xs (without replacement).
permutationsN :: Int -> [a] -> [[a]]
permutationsN 0 _  = [[]]
permutationsN _ [] = []
permutationsN n xs = [ y : ys | y <- xs, ys <- permutationsN (n - 1) (delete y xs) ]
