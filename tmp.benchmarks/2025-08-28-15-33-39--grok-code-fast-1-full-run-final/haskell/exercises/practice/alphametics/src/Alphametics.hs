module Alphametics (solve) where

import Data.List (nub, permutations, sort)
import Data.Char (toUpper)
import Data.Maybe (fromJust)

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c s = let (a, b) = break (== c) s
              in if null b then [a] else a : splitOn c (tail b)

solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  let clean = map toUpper $ filter (/= ' ') puzzle
      (left, eqRight) = break (== '=') clean
      sumWord = tail eqRight
      addends = splitOn '+' left
      allWords = addends ++ [sumWord]
      letters = nub $ concat allWords
      n = length letters
      sortedLetters = sort letters
      leadingLetters = nub $ map head allWords
      candidates = [zip sortedLetters (take n ds) | ds <- permutations [0..9]]
      valid = filter (\assign -> all (\c -> (fromJust $ lookup c assign) /= 0) leadingLetters) candidates
      solutions = filter (\assign ->
        let wordToNum w = foldl (\acc c -> acc * 10 + (fromJust $ lookup c assign)) 0 w
            addendNums = map wordToNum addends
            sumNum = wordToNum sumWord
        in sum addendNums == sumNum
      ) valid
  in case solutions of
     [] -> Nothing
     (sol : _) -> Just sol
