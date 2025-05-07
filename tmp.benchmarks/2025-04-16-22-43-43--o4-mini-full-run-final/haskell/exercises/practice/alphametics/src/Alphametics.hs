module Alphametics (solve) where

import Data.Char (isSpace)
import Data.List (nub, permutations, sort, lookup)
import Data.Maybe (listToMaybe, fromJust)
import Control.Monad (guard)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe solutions
  where
    (lhs, rhs)   = parse puzzle
    wordsAll     = rhs : lhs
    letters      = nub $ concat wordsAll
    leadingChars = nub [ head w | w <- wordsAll, length w > 1 ]

    solutions = do
      -- choose exactly as many digits as there are letters
      digitSet <- combinations (length letters) [0..9]
      -- try all orderings of that set
      perm     <- permutations digitSet
      let mapping = zip letters perm

      -- no leading letter is zero
      guard $ all (\c -> fromJust (lookup c mapping) /= 0) leadingChars

      -- convert a word to its numeric value
      let wordValue w =
            foldl (\acc c -> acc * 10 + fromJust (lookup c mapping)) 0 w

      -- check the sum
      guard $ sum (map wordValue lhs) == wordValue rhs

      return (sort mapping)

-- generate all k‑element combinations of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise = map (x:) (combinations (k - 1) xs)
             ++ combinations k xs

-- Parse "WORD1 + WORD2 = RESULT" into (["WORD1","WORD2"], "RESULT")
parse :: String -> ([String], String)
parse s =
  let (l, _ : r) = break (=='=') s
      lhsWords   = splitOn '+' l
      strip      = f . f
        where f = reverse . dropWhile isSpace
      clean = strip . filter (not . isSpace)
  in (map clean lhsWords, clean r)

-- Split a string on a delimiter
splitOn :: Char -> String -> [String]
splitOn delim str =
  case break (== delim) str of
    (h, _ : t) -> h : splitOn delim t
    (h, "")    -> [h]
