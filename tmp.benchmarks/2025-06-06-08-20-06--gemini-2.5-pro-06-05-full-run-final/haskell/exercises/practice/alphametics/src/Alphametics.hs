module Alphametics (solve) where

import Data.Char (isUpper)
import Data.List (find, foldl', nub, permutations)
import Data.Maybe (fromJust)

solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  let (summands, result) = parse puzzle
      allWords = summands ++ [result]
      letters = nub $ filter isUpper puzzle
      leadingLetters = nub [head w | w <- allWords, length w > 1]
   in if length letters > 10
        then Nothing
        else find (isValid (summands, result) leadingLetters . zip letters) (permutations [0 .. 9])

parse :: String -> ([String], String)
parse puzzle =
  let parts = words $ map (\c -> if c `elem` "=+-" then ' ' else c) puzzle
      summands = init parts
      result = last parts
   in (summands, result)

eval :: [(Char, Int)] -> String -> Integer
eval mapping word = foldl' (\acc c -> acc * 10 + toInteger (fromJust $ lookup c mapping)) 0 word

isValid :: ([String], String) -> [Char] -> [(Char, Int)] -> Bool
isValid (summands, result) leadingLetters mapping =
  not (any (\c -> lookup c mapping == Just 0) leadingLetters)
    && sum (map (eval mapping) summands) == eval mapping result
